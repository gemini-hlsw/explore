// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.derived.*
import cats.implicits.*
import eu.timepit.refined.cats.given
import explore.model.enums.GroupWarning
import explore.model.syntax.all.*
import lucuma.core.enums.ObservationValidationCode
import lucuma.core.enums.ObservationWorkflowState
import lucuma.core.enums.ScienceBand
import lucuma.core.model.Configuration
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.PartnerLink
import lucuma.core.model.ProgramReference
import lucuma.core.model.ProposalReference
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.model.TimingWindow
import lucuma.core.model.UnnormalizedSED
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
  attachments:           AttachmentList,
  programs:              ProgramInfoList,
  configurationRequests: ConfigurationRequestList
) derives Eq:
  lazy val proposalIsSubmitted =
    optProgramDetails.exists(_.proposalStatus === ProposalStatus.Submitted)
  lazy val proposalIsAccepted  =
    optProgramDetails.exists(_.proposalStatus === ProposalStatus.Accepted)

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

  lazy val hasUndefinedObservations: Boolean =
    observations.values.exists(_.workflow.value.state === ObservationWorkflowState.Undefined)

  lazy val hasDefinedObservations: Boolean =
    observations.values.exists(_.workflow.value.state === ObservationWorkflowState.Defined)

  lazy val obsAttachmentAssignments: ObsAttachmentAssignmentMap =
    observations.toList
      .flatMap((obsId, obs) => obs.attachmentIds.map(_ -> obsId))
      .groupMap(_._1)(_._2)
      .view
      .mapValues(obsIds => SortedSet.from(obsIds))
      .toMap

  lazy val targetAttachmentAssignments: TargetAttachmentAssignmentMap =
    targets.toList
      .map((targetId, target) =>
        SourceProfile.unnormalizedSED.some
          .andThen(UnnormalizedSED.userDefinedAttachment)
          .getOption(target.sourceProfile)
          .map(u => u.attachmentId -> targetId)
      )
      .flattenOption
      .groupMap(_._1)(_._2)
      .view
      .mapValues(SortedSet.from)
      .toMap

  lazy val targetsWithObs: TargetWithObsList =
    targets.map((targetId, target) =>
      targetId -> TargetWithObs(target, targetObservations.get(targetId).orEmpty)
    )

  lazy val calibrationObservationIds: Set[Observation.Id] =
    observations.values.filter(_.isCalibration).map(_.id).toSet

  lazy val nonCalibrationObservations: List[Observation] =
    observations.values.filterNot(_.isCalibration).toList

  lazy val constraintGroups: ConstraintGroupList =
    SortedMap.from:
      nonCalibrationObservations
        .map(obs => obs.constraints -> obs.id)
        .groupMap(_._1)(_._2)
        .map((c, obsIds) => ObsIdSet.of(obsIds.head, obsIds.tail.toList*) -> c)

  lazy val schedulingGroups: SchedulingGroupList =
    SortedMap.from:
      nonCalibrationObservations
        .map(obs => obs.timingWindows.sorted -> obs.id)
        .groupMap(_._1)(_._2)
        .map((tws, obsIds) => ObsIdSet.of(obsIds.head, obsIds.tail.toList*) -> tws.sorted)

  lazy val observingModeGroups: ObservingModeGroupList =
    SortedMap.from:
      nonCalibrationObservations
        .map(obs => obs.observingModeSummary -> obs.id)
        .groupMap(_._1)(_._2)
        .map((mode, obsIds) => ObsIdSet.of(obsIds.head, obsIds.tail.toList*) -> mode)

  lazy val programOrProposalReference: Option[String] =
    ProgramSummaries.programReference
      .getOption(this)
      .map(_.label)
      .orElse(ProgramSummaries.proposalReference.getOption(this).map(_.label))

  lazy val allocatedScienceBands: SortedSet[ScienceBand] =
    optProgramDetails.foldMap(_.allocations.scienceBands)

  lazy val obs4ConfigRequests: Map[ConfigurationRequest.Id, List[Observation]] =
    observations.values.toList
      .filterNot(_.isCalibration)
      .foldRight(Map.empty[ConfigurationRequest.Id, List[Observation]]): (obs, accum) =>
        obs.configurationRequestIds.foldRight(accum): (crId, innerAccum) =>
          innerAccum.updatedWith(crId)(_.fold(List(obs))(_ :+ obs).some)

  lazy val configsWithoutRequests: Map[Configuration, NonEmptyList[Observation]] =
    val l = observations.values.toList
      .filter: o =>
        o.workflow.value.state =!= ObservationWorkflowState.Inactive &&
          o.calibrationRole.isEmpty &&
          o.configuration.fold(false): config =>
            o.hasNotRequestedCode ||
              // Not explicitly denied
              (o.hasDeniedValidationCode &&
                configurationRequests.forall((_, v) => v.configuration =!= config))
    l.foldRight(Map.empty[Configuration, NonEmptyList[Observation]])((o, m) =>
      m.updatedWith(o.configuration.get)(_.fold(NonEmptyList.one(o))(nel => nel :+ o).some)
    )

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

  // Limit how deep we check group warnings. At some point the odb will probably need to limit tree depth,
  // but it currently doesn not do so. Deep trees will kill time calculation and scheduling...
  private val maxGroupCheckDepth = 10

  private def allObservationsForGroup(groupId: Group.Id, depth: Int = 0): List[Observation] =
    groupsChildren
      .get(groupId.some)
      .fold(List.empty): children =>
        children.flatMap {
          case Left(obs)    => List(obs)
          case Right(group) =>
            if (depth < maxGroupCheckDepth) allObservationsForGroup(group.id, depth + 1)
            else List.empty
        }

  lazy val allObservationsForGroups: List[(Group, List[Observation])] =
    groups.values
      .filterNot(_.system)
      .map(group => (group, allObservationsForGroup(group.id)))
      .toList

  lazy val groupWarnings: Map[Group.Id, NonEmptySet[GroupWarning]] =
    extension (b:   Boolean)
      def mkSet(gw: GroupWarning): Set[GroupWarning] = if (b) Set(gw) else Set.empty
    val ignoreStates: Set[ObservationWorkflowState]  =
      Set(ObservationWorkflowState.Inactive, ObservationWorkflowState.Undefined)

    allObservationsForGroups
      .map: (group, obses) =>
        val undefWarning =
          obses
            .exists(_.workflow.value.state === ObservationWorkflowState.Undefined)
            .mkSet(GroupWarning.UndefinedObservations)
        val unapproved   =
          obses
            .exists(_.workflow.value.state === ObservationWorkflowState.Unapproved)
            .mkSet(GroupWarning.UnapprovedObservations)

        val obs2Check =
          NonEmptyList.fromList(obses.filterNot(o => ignoreStates.contains(o.workflow.value.state)))

        val moreWarnings = obs2Check.fold(Set.empty): nel =>
          val bandMismatch = // for all AND groups
            (group.isAnd && nel.tail.exists(_.scienceBand =!= nel.head.scienceBand))
              .mkSet(GroupWarning.BandMismatch)
          val siteMismatch = // for "consecutive" AND groups
            // maximum interval starts out as empty
            (group.isAnd && group.maximumInterval.forall(_.isZero) &&
              nel.tail.exists(_.site =!= nel.head.site)).mkSet(GroupWarning.SiteMismatch)
          bandMismatch ++ siteMismatch

        NonEmptySet
          .fromSet(SortedSet.from(undefWarning ++ unapproved ++ moreWarnings))
          .map(nes => (group.id, nes))
      .flattenOption
      .toMap

object ProgramSummaries:
  val optProgramDetails: Lens[ProgramSummaries, Option[ProgramDetails]]       =
    Focus[ProgramSummaries](_.optProgramDetails)
  val proposal: Optional[ProgramSummaries, Option[Proposal]]                  =
    optProgramDetails.some.andThen(ProgramDetails.proposal)
  val targets: Lens[ProgramSummaries, TargetList]                             = Focus[ProgramSummaries](_.targets)
  val observations: Lens[ProgramSummaries, ObservationList]                   =
    Focus[ProgramSummaries](_.observations)
  val groups: Lens[ProgramSummaries, GroupList]                               = Focus[ProgramSummaries](_.groups)
  val attachments: Lens[ProgramSummaries, AttachmentList]                     = Focus[ProgramSummaries](_.attachments)
  val programs: Lens[ProgramSummaries, ProgramInfoList]                       = Focus[ProgramSummaries](_.programs)
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
    optProgramDetails: Option[ProgramDetails],
    targetList:        List[TargetWithId],
    obsList:           List[Observation],
    groupList:         List[Group],
    attachments:       List[Attachment],
    programs:          List[ProgramInfo],
    configRequests:    List[ConfigurationRequest]
  ): ProgramSummaries =
    ProgramSummaries(
      optProgramDetails,
      targetList.toSortedMap(_.id, _.target),
      obsList.toSortedMap(_.id),
      groupList.toSortedMap(_.id),
      attachments.toSortedMap(_.id),
      programs.toSortedMap(_.id),
      configRequests.toSortedMap(_.id)
    )
