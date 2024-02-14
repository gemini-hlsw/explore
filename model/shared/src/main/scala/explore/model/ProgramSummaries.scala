// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.data.NonEmptySet
import cats.derived.*
import cats.implicits.*
import crystal.Pot
import explore.data.KeyedIndexedList
import explore.model.syntax.all.*
import lucuma.core.model.ObsAttachment
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.schemas.enums.ProposalStatus
import lucuma.schemas.model.TargetWithId
import monocle.Focus
import monocle.Lens

import java.util.UUID
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

case class ProgramSummaries(
  optProgramDetails:   Option[ProgramDetails],
  targets:             TargetList,
  observations:        ObservationList,
  groups:              GroupList,
  obsAttachments:      ObsAttachmentList,
  proposalAttachments: List[ProposalAttachment],
  programs:            ProgramInfoList,
  programTimesPot:     (UUID, Pot[ProgramTimes]), // access via `progTimes`
  obsExecutionPots:    ObservationExecutionMap    // see extension methods on ObservationExecutionMap
) derives Eq:
  lazy val proposalIsSubmitted =
    optProgramDetails.exists(_.proposalStatus === ProposalStatus.Submitted)

  lazy val asterismGroups: AsterismGroupList =
    SortedMap.from(
      observations.values
        .map(obs => obs.id -> obs.scienceTargetIds)
        .groupMap(_._2)(_._1)
        .map((targets, observations) =>
          ObsIdSet(NonEmptySet.of(observations.head, observations.tail.toList*)) -> SortedSet
            .from(targets)
        )
    )

  lazy val targetObservations: Map[Target.Id, SortedSet[Observation.Id]] =
    observations.values
      .flatMap(obs => obs.scienceTargetIds.map(targetId => targetId -> obs.id))
      .groupMap(_._1)(_._2)
      .view
      .mapValues(obsIds => SortedSet.from(obsIds))
      .toMap

  lazy val obsAttachmentAssignments: ObsAttachmentAssignmentMap =
    observations.values
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
    SortedMap.from(
      observations.values
        .groupMap(_.constraints)(_.id)
        .map((c, obsIds) => ObsIdSet.of(obsIds.head, obsIds.tail.toList*) -> c)
    )

  lazy val schedulingGroups: SchedulingGroupList =
    SortedMap.from(
      observations.values
        .groupMap(_.timingWindows.sorted)(_.id)
        .map((tws, obsIds) => ObsIdSet.of(obsIds.head, obsIds.tail.toList*) -> tws.sorted)
    )

  lazy val programTimes: Pot[ProgramTimes] = programTimesPot._2

  def cloneObsWithTargets(
    originalId: Observation.Id,
    clonedId:   Observation.Id,
    targetIds:  List[Target.Id]
  ): Option[ObsSummary] =
    observations
      .getValue(originalId)
      .map(_.copy(id = clonedId, scienceTargetIds = SortedSet.from(targetIds)))

  def insertObs(obsSummary: ObsSummary): ProgramSummaries =
    ProgramSummaries.observations.modify(
      _.inserted(obsSummary.id, obsSummary, observations.length)
    )(this)

  def removeObs(obsId: Observation.Id): ProgramSummaries =
    ProgramSummaries.observations.modify(_.removed(obsId))(this)

object ProgramSummaries:
  val optProgramDetails: Lens[ProgramSummaries, Option[ProgramDetails]]     =
    Focus[ProgramSummaries](_.optProgramDetails)
  val targets: Lens[ProgramSummaries, TargetList]                           = Focus[ProgramSummaries](_.targets)
  val observations: Lens[ProgramSummaries, ObservationList]                 =
    Focus[ProgramSummaries](_.observations)
  val groups: Lens[ProgramSummaries, GroupList]                             = Focus[ProgramSummaries](_.groups)
  val obsAttachments: Lens[ProgramSummaries, ObsAttachmentList]             =
    Focus[ProgramSummaries](_.obsAttachments)
  val proposalAttachments: Lens[ProgramSummaries, List[ProposalAttachment]] =
    Focus[ProgramSummaries](_.proposalAttachments)
  val programs: Lens[ProgramSummaries, ProgramInfoList]                     = Focus[ProgramSummaries](_.programs)
  val programTimesPot: Lens[ProgramSummaries, (UUID, Pot[ProgramTimes])]    =
    Focus[ProgramSummaries](_.programTimesPot)
  val obsExecutionPots: Lens[ProgramSummaries, ObservationExecutionMap]     =
    Focus[ProgramSummaries](_.obsExecutionPots)

  def fromLists(
    optProgramDetails:   Option[ProgramDetails],
    targetList:          List[TargetWithId],
    obsList:             List[ObsSummary],
    groups:              List[GroupElement],
    obsAttachments:      List[ObsAttachment],
    proposalAttachments: List[ProposalAttachment],
    programs:            List[ProgramInfo],
    programTimesPot:     (UUID, Pot[ProgramTimes]),
    obsExecutionPots:    Map[Observation.Id, (UUID, Pot[Execution])]
  ): ProgramSummaries =
    ProgramSummaries(
      optProgramDetails,
      targetList.toSortedMap(_.id, _.target),
      KeyedIndexedList.fromList(obsList, ObsSummary.id.get),
      groups,
      obsAttachments.toSortedMap(_.id),
      proposalAttachments,
      programs.toSortedMap(_.id),
      programTimesPot,
      ObservationExecutionMap(obsExecutionPots)
    )
