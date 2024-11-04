// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.derived.*
import cats.implicits.*
import crystal.Pot
import explore.data.KeyedIndexedList
import explore.model.syntax.all.*
import lucuma.core.enums.ScienceBand
import lucuma.core.model.Configuration
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Group
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
  groups:                GroupTree,
  systemGroups:          GroupTree,
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
      observations.toList
        .map(obs => obs.id -> obs.scienceTargetIds)
        .groupMap(_._2)(_._1)
        .map: (targets, observations) =>
          ObsIdSet(NonEmptySet.of(observations.head, observations.tail.toList*)) -> SortedSet
            .from(targets)

  lazy val targetObservations: Map[Target.Id, SortedSet[Observation.Id]] =
    observations.toList
      .flatMap(obs => obs.scienceTargetIds.map(targetId => targetId -> obs.id))
      .groupMap(_._1)(_._2)
      .view
      .mapValues(obsIds => SortedSet.from(obsIds))
      .toMap

  lazy val obsTargets: Map[Observation.Id, TargetList] =
    observations.toList
      .map: obs =>
        obs.id ->
          SortedMap.from:
            obs.scienceTargetIds.toList
              .map(tid => targets.get(tid).map(t => tid -> t))
              .flattenOption
      .toMap

  lazy val obsAttachmentAssignments: ObsAttachmentAssignmentMap =
    observations.toList
      .flatMap(obs => obs.attachmentIds.map(_ -> obs.id))
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
        .groupMap(_.constraints)(_.id)
        .map((c, obsIds) => ObsIdSet.of(obsIds.head, obsIds.tail.toList*) -> c)

  lazy val schedulingGroups: SchedulingGroupList =
    SortedMap.from(
      observations.toList
        .groupMap(_.timingWindows.sorted)(_.id)
        .map((tws, obsIds) => ObsIdSet.of(obsIds.head, obsIds.tail.toList*) -> tws.sorted)
    )

  lazy val calibrationObservations: Set[Observation.Id] =
    observations.toList.filter(_.isCalibration).map(_.id).toSet

  lazy val programOrProposalReference: Option[String] =
    ProgramSummaries.programReference
      .getOption(this)
      .map(_.label)
      .orElse(ProgramSummaries.proposalReference.getOption(this).map(_.label))

  lazy val allocatedScienceBands: SortedSet[ScienceBand] =
    optProgramDetails.foldMap(_.allocations.scienceBands)

  lazy val obs4ConfigRequests: Map[ConfigurationRequest.Id, List[Observation]] =
    configurationRequests
      .map((crId, cr) =>
        val obs = observations.toList
          .filter(_.configuration.fold(false)(cr.configuration.subsumes))
        (crId, obs)
      )
      .toMap

  lazy val configsWithoutRequests: Map[Configuration, NonEmptyList[Observation]] =
    val l = observations.toList
      .filter: o =>
        o.configuration.fold(false): config =>
          o.hasNotRequestedCode ||
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
      .getValue(originalId)
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
    ProgramSummaries.observations.modify(
      _.inserted(observation.id, observation, observations.length)
    )(this)

  def removeObs(obsId: Observation.Id): ProgramSummaries =
    ProgramSummaries.observations.modify(_.removed(obsId))(this)

  def cloneTargetForObservations(
    originalId: Target.Id,
    clone:      TargetWithId,
    obsIds:     ObsIdSet
  ): ProgramSummaries =
    val obs = obsIds.idSet.foldLeft(observations)((list, obsId) =>
      list.updatedValueWith(obsId, Observation.scienceTargetIds.modify(_ - originalId + clone.id))
    )
    val ts  = targets + (clone.id -> clone.target)
    copy(observations = obs, targets = ts)

  def unCloneTargetForObservations(
    originalId: Target.Id,
    cloneId:    Target.Id,
    obsIds:     ObsIdSet
  ): ProgramSummaries =
    val obs = obsIds.idSet.foldLeft(observations)((list, obsId) =>
      list.updatedValueWith(obsId, Observation.scienceTargetIds.modify(_ + originalId - cloneId))
    )
    val ts  = targets - cloneId
    copy(observations = obs, targets = ts)

object ProgramSummaries:
  val optProgramDetails: Lens[ProgramSummaries, Option[ProgramDetails]]       =
    Focus[ProgramSummaries](_.optProgramDetails)
  val proposal: Optional[ProgramSummaries, Option[Proposal]]                  =
    optProgramDetails.some.andThen(ProgramDetails.proposal)
  val targets: Lens[ProgramSummaries, TargetList]                             = Focus[ProgramSummaries](_.targets)
  val observations: Lens[ProgramSummaries, ObservationList]                   =
    Focus[ProgramSummaries](_.observations)
  val groups: Lens[ProgramSummaries, GroupTree]                               = Focus[ProgramSummaries](_.groups)
  val systemGroups: Lens[ProgramSummaries, GroupTree]                         = Focus[ProgramSummaries](_.systemGroups)
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
    groups:              GroupTree,
    systemGroups:        GroupTree,
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
      KeyedIndexedList.fromList(obsList, Observation.id.get),
      groups,
      systemGroups,
      obsAttachments.toSortedMap(_.id),
      proposalAttachments,
      programs.toSortedMap(_.id),
      programTimesPot,
      ObservationExecutionMap(obsExecutionPots),
      GroupTimeRangeMap(groupTimeRangePots),
      configRequests.toSortedMap(_.id)
    )
