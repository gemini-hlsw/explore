// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.derived.*
import cats.implicits.*
import crystal.Pot
import eu.timepit.refined.cats.given
import explore.model.syntax.all.*
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ScienceBand
import lucuma.core.model.Configuration
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ObsAttachment
import lucuma.core.model.PartnerLink
import lucuma.core.model.ProgramReference
import lucuma.core.model.ProposalReference
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.schemas.enums.ProposalStatus
import lucuma.schemas.model.TargetWithId
import monocle.Focus
import monocle.Lens
import monocle.Optional

import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

case class ProgramSummaries(
  optProgramDetails:     Option[ProgramDetails],
  targets:               TargetList,
  observations:          ObservationList,
  groups:                GroupList,
  obsAttachments:        ObsAttachmentList,
  proposalAttachments:   List[ProposalAttachment],
  programs:              ProgramInfoList,
  programTimesPot:       Pot[ProgramTimes],
  obsExecutionPots:      ObservationExecutionMap,
  groupTimeRangePots:    GroupTimeRangeMap,
  configurationRequests: ConfigurationRequestList
) derives Eq:
  lazy val proposalIsSubmitted =
    optProgramDetails.exists(_.proposalStatus === ProposalStatus.Submitted)

  lazy val proposalId: Option[ProposalReference] =
    optProgramDetails.flatMap(_.proposal.flatMap(_.reference))

  lazy val asterismGroups: AsterismGroupList =
    SortedMap.from:
      observations.view
        .mapValues(_.scienceTargetIds)
        .groupMap(_._2)(_._1)
        .map: (targets, observations) =>
          ObsIdSet(NonEmptySet.of(observations.head, observations.tail.toList*)) -> SortedSet
            .from(targets)

  lazy val targetObservations: Map[Target.Id, SortedSet[Observation.Id]] =
    observations.toList
      .flatMap((obsId, obs) => obs.scienceTargetIds.map(targetId => targetId -> obsId))
      .groupMap(_._1)(_._2)
      .view
      .mapValues(obsIds => SortedSet.from(obsIds))
      .toMap

  lazy val obsTargets: Map[Observation.Id, TargetList] =
    observations.view
      .mapValues: obs =>
        SortedMap.from:
          obs.scienceTargetIds.toList
            .map(tid => targets.get(tid).map(t => tid -> t))
            .flattenOption
      .toMap

  lazy val obsAttachmentAssignments: ObsAttachmentAssignmentMap =
    observations.toList
      .flatMap((obsId, obs) => obs.attachmentIds.map(_ -> obsId))
      .groupMap(_._1)(_._2)
      .view
      .mapValues(obsIds => SortedSet.from(obsIds))
      .toMap

  // Might not be used after all
  lazy val targetsWithObs: TargetWithObsList =
    targets.map((targetId, target) =>
      targetId -> TargetWithObs(target, targetObservations.get(targetId).orEmpty)
    )

  lazy val constraintGroups: ConstraintGroupList =
    SortedMap.from:
      observations.toList
        .map((obsId, obs) => obs.constraints -> obsId)
        .groupMap(_._1)(_._2)
        .map((c, obsIds) => ObsIdSet.of(obsIds.head, obsIds.tail.toList*) -> c)

  lazy val schedulingGroups: SchedulingGroupList =
    SortedMap.from(
      observations.toList
        .map((obsId, obs) => obs.timingWindows.sorted -> obsId)
        .groupMap(_._1)(_._2)
        .map((tws, obsIds) => ObsIdSet.of(obsIds.head, obsIds.tail.toList*) -> tws.sorted)
    )

  lazy val calibrationObservations: Set[Observation.Id] =
    observations.values.filter(_.isCalibration).map(_.id).toSet

  lazy val programOrProposalReference: Option[String] =
    ProgramSummaries.programReference
      .getOption(this)
      .map(_.label)
      .orElse(ProgramSummaries.proposalReference.getOption(this).map(_.label))

  lazy val allocatedScienceBands: SortedSet[ScienceBand] =
    optProgramDetails.foldMap(_.allocations.scienceBands)

  lazy val obs4ConfigRequests: Map[ConfigurationRequest.Id, List[Observation]] =
    configurationRequests
      .map: (crId, cr) =>
        val obsList: List[Observation] =
          observations.values.toList
            .filter: obs =>
              // keep inactive ones here.
              obs.calibrationRole.isEmpty &&
                obs.configuration.fold(false)(cr.configuration.subsumes)
        (crId, obsList)
      .toMap

  lazy val configsWithoutRequests: Map[Configuration, NonEmptyList[Observation]] =
    val l = observations.values.toList
      .filter: o =>
        o.workflow.state =!= ObservationWorkflowState.Inactive &&
          o.calibrationRole.isEmpty &&
          o.configuration.fold(false): config =>
            o.hasNotRequestedCode ||
              // Not explicitly denied
              (o.hasDeniedValidationCode &&
                configurationRequests.forall((_, v) => v.configuration =!= config))
    l.foldRight(Map.empty[Configuration, NonEmptyList[Observation]])((o, m) =>
      m.updatedWith(o.configuration.get)(_.fold(NonEmptyList.one(o))(nel => nel :+ o).some)
    )

  def getObsClone(
    originalId:        Observation.Id,
    clonedId:          Observation.Id,
    withTargets:       Option[List[Target.Id]] = none,
    withConstraintSet: Option[ConstraintSet] = none,
    withTimingWindows: Option[List[TimingWindow]] = none
  ): Option[Observation] =
    observations
      .get(originalId)
      .map:
        Observation.id.replace(clonedId) >>>
          withTargets
            .map(tids => Observation.scienceTargetIds.replace(SortedSet.from(tids)))
            .getOrElse(identity) >>>
          withConstraintSet
            .map(cs => Observation.constraints.replace(cs))
            .getOrElse(identity) >>>
          withTimingWindows
            .map(tws => Observation.timingWindows.replace(tws))
            .getOrElse(identity)

  def insertObs(observation: Observation): ProgramSummaries =
    ProgramSummaries.observations.modify(_ + (observation.id -> observation))(this)

  def removeObs(obsId: Observation.Id): ProgramSummaries =
    ProgramSummaries.observations.modify(_.removed(obsId))(this)

  def cloneTargetForObservations(
    originalId: Target.Id,
    clone:      TargetWithId,
    obsIds:     ObsIdSet
  ): ProgramSummaries =
    val obs: ObservationList = obsIds.idSet.foldLeft(observations): (map, obsId) =>
      map.updatedWith(obsId)(_.map(Observation.scienceTargetIds.modify(_ - originalId + clone.id)))
    val ts: TargetList       = targets + (clone.id -> clone.target)
    copy(observations = obs, targets = ts)

  def unCloneTargetForObservations(
    originalId: Target.Id,
    cloneId:    Target.Id,
    obsIds:     ObsIdSet
  ): ProgramSummaries =
    val obs: ObservationList = obsIds.idSet.foldLeft(observations): (map, obsId) =>
      map.updatedWith(obsId)(_.map(Observation.scienceTargetIds.modify(_ + originalId - cloneId)))
    val ts: TargetList       = targets - cloneId
    copy(observations = obs, targets = ts)

  def parentGroups(id: Either[Observation.Id, Group.Id]): List[Group.Id] =
    id.fold(
      observations.get(_).flatMap(_.groupId),
      groups.get(_).flatMap(_.parentId)
    ).map: groupId =>
      groupId +: parentGroups(groupId.asRight)
    .orEmpty

  lazy val groupsChildren: Map[Option[Group.Id], List[Either[Observation, Group]]] =
    val groupsByParent: List[(Option[Group.Id], Either[Observation, Group])] =
      groups.values.toList
        .map: group =>
          group.parentId -> group.asRight

    val obsByParent: List[(Option[Group.Id], Either[Observation, Group])] =
      observations.values.toList
        .map: obs =>
          obs.groupId -> obs.asLeft

    (groupsByParent ++ obsByParent)
      .groupMap(_._1)(_._2)
      .view
      .mapValues:
        _.sortBy:
          _.fold(_.groupIndex, _.parentIndex)
      .toMap
  end groupsChildren

object ProgramSummaries:
  val optProgramDetails: Lens[ProgramSummaries, Option[ProgramDetails]]       =
    Focus[ProgramSummaries](_.optProgramDetails)
  val proposal: Optional[ProgramSummaries, Option[Proposal]]                  =
    optProgramDetails.some.andThen(ProgramDetails.proposal)
  val targets: Lens[ProgramSummaries, TargetList]                             = Focus[ProgramSummaries](_.targets)
  val observations: Lens[ProgramSummaries, ObservationList]                   =
    Focus[ProgramSummaries](_.observations)
  val groups: Lens[ProgramSummaries, GroupList]                               = Focus[ProgramSummaries](_.groups)
  val obsAttachments: Lens[ProgramSummaries, ObsAttachmentList]               =
    Focus[ProgramSummaries](_.obsAttachments)
  val proposalAttachments: Lens[ProgramSummaries, List[ProposalAttachment]]   =
    Focus[ProgramSummaries](_.proposalAttachments)
  val programs: Lens[ProgramSummaries, ProgramInfoList]                       = Focus[ProgramSummaries](_.programs)
  val programTimesPot: Lens[ProgramSummaries, Pot[ProgramTimes]]              =
    Focus[ProgramSummaries](_.programTimesPot)
  val obsExecutionPots: Lens[ProgramSummaries, ObservationExecutionMap]       =
    Focus[ProgramSummaries](_.obsExecutionPots)
  val groupTimeRangePots: Lens[ProgramSummaries, GroupTimeRangeMap]           =
    Focus[ProgramSummaries](_.groupTimeRangePots)
  val configurationRequests: Lens[ProgramSummaries, ConfigurationRequestList] =
    Focus[ProgramSummaries](_.configurationRequests)

  val programReference: Optional[ProgramSummaries, ProgramReference] =
    optProgramDetails.some.andThen(ProgramDetails.reference.some)

  val proposalReference: Optional[ProgramSummaries, ProposalReference] =
    optProgramDetails.some.andThen:
      ProgramDetails.proposal.some.andThen(Proposal.reference.some)

  val piPartner: Optional[ProgramSummaries, PartnerLink] =
    ProgramSummaries.optProgramDetails.some
      .andThen(ProgramDetails.piPartner.some)

  def fromLists(
    optProgramDetails:   Option[ProgramDetails],
    targetList:          List[TargetWithId],
    obsList:             List[Observation],
    groupList:           List[Group],
    obsAttachments:      List[ObsAttachment],
    proposalAttachments: List[ProposalAttachment],
    programs:            List[ProgramInfo],
    programTimesPot:     Pot[ProgramTimes],
    obsExecutionPots:    Map[Observation.Id, Pot[Execution]],
    groupTimeRangePots:  Map[Group.Id, Pot[Option[ProgramTimeRange]]],
    configRequests:      List[ConfigurationRequest]
  ): ProgramSummaries =
    ProgramSummaries(
      optProgramDetails,
      targetList.toSortedMap(_.id, _.target),
      obsList.toSortedMap(_.id),
      groupList.toSortedMap(_.id),
      obsAttachments.toSortedMap(_.id),
      proposalAttachments,
      programs.toSortedMap(_.id),
      programTimesPot,
      ObservationExecutionMap(obsExecutionPots),
      GroupTimeRangeMap(groupTimeRangePots),
      configRequests.toSortedMap(_.id)
    )
