// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Eq
import cats.Order
import cats.Order.given
import cats.effect.Async
import cats.implicits.*
import clue.FetchClient
import clue.data.syntax.*
import explore.DefaultErrorPolicy
import explore.model.AsterismGroup
import explore.model.ObsIdSet
import explore.model.ObsSummary
import cats.data.NonEmptySet

import explore.model.TargetWithObs
import explore.model.syntax.all.*
import japgolly.scalajs.react.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.*
import lucuma.schemas.odb.input.*
import lucuma.ui.reusability.given
import monocle.Focus
import monocle.Getter
import queries.common.AsterismQueriesGQL.*
import queries.common.ObsQueriesGQL.*
import explore.model.TargetList
import explore.model.ObservationList
import explore.model.AsterismGroupList
import explore.model.TargetWithObsList
import explore.model.ConstraintGroupList

import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
// import explore.model.Focused.obsSet
// import explore.model.TargetWithIdAndObs.targetWithObs
import explore.data.KeyedIndexedList
import explore.model.AsterismIds
// import lucuma.core.model.ConstraintSet
// import lucuma.schemas.model.TargetWithId
// import monocle.Iso
// import lucuma.core.optics.SplitEpi
// import cats.data.NonEmptyList
// import java.time.Instant
// import lucuma.core.model.ObjectTracking
// import scala.annotation.targetName
// import explore.model.extensions.*

object AsterismQueries:
  // The default cats ordering for sorted set sorts by size first, then contents. That's not what we want.
  // This is used for sorting the AsterismGroupObsList. If we change to sort by name or something
  // else, we can remove this.
  // TODO IS this necessary now?
  // given Order[ObsIdSet] = ObsIdSet.given_Order_ObsIdSet

  case class ProgramSummaries(
    targets:      TargetList,
    observations: ObservationList
  ) {
    lazy val asterismGroups: AsterismGroupList =
      SortedMap.from(
        observations.values
          .map(obs => obs.id -> obs.scienceTargetIds)
          .groupMap(_._2)(_._1)
          .map((targets, observations) =>
            ObsIdSet(NonEmptySet.of(observations.head, observations.tail.toList: _*)) -> SortedSet
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

    // Might not be used after all
    lazy val targetsWithObs: TargetWithObsList =
      targets.map((targetId, target) =>
        targetId -> TargetWithObs(target, targetObservations.get(targetId).orEmpty)
      )

    lazy val constraintGroups: ConstraintGroupList =
      SortedMap.from(
        observations.values
          .groupMap(_.constraints)(_.id)
          .map((c, obsIds) => ObsIdSet.of(obsIds.head, obsIds.tail.toList: _*) -> c)
      )

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

    def removeObsWithTargets(
      obsId:     Observation.Id,
      targetIds: SortedSet[Target.Id]
    ): ProgramSummaries =
      ProgramSummaries.observations.modify(_.removed(obsId))(this)
  }

  object ProgramSummaries:
    val targets      = Focus[ProgramSummaries](_.targets)
    val observations = Focus[ProgramSummaries](_.observations)

  // Some helper methods on AsterismGroupList
  extension (self: AsterismGroupList)
    def findContainingObsIds(obsIds: ObsIdSet): Option[AsterismGroup] =
      self.find { case (ids, _) => obsIds.subsetOf(ids) }.map(AsterismGroup.fromTuple)

    def findWithTargetIds(targetIds: SortedSet[Target.Id]): Option[AsterismGroup] =
      self.find { case (_, grpIds) => grpIds === targetIds }.map(AsterismGroup.fromTuple)

  private val queryToAsterismGroupWithObsGetter
    : Getter[AsterismGroupObsQuery.Data, ProgramSummaries] = data =>
    ProgramSummaries(
      data.targets.matches.toSortedMap(_.id, _.target),
      KeyedIndexedList.fromList(data.observations.matches, ObsSummary.id.get)
    )

  extension (self: AsterismGroupObsQuery.Data.type)
    def asAsterismGroupWithObs = queryToAsterismGroupWithObsGetter

  def replaceAsterism[F[_]: Async](
    programId: Program.Id,
    obsIds:    List[Observation.Id],
    targetIds: List[Target.Id]
  )(using FetchClient[F, ?, ObservationDB]) =
    val input = UpdateObservationsInput(
      programId = programId,
      WHERE = obsIds.toWhereObservation.assign,
      SET = ObservationPropertiesInput(
        targetEnvironment = TargetEnvironmentInput(asterism = targetIds.assign).assign
      )
    )
    UpdateObservationMutation[F].execute(input).void

  def addTargetsToAsterisms[F[_]: Async](
    programId: Program.Id,
    obsIds:    List[Observation.Id],
    targetIds: List[Target.Id]
  )(using FetchClient[F, ?, ObservationDB]) =
    val input = UpdateAsterismsInput(
      programId = programId,
      WHERE = obsIds.toWhereObservation.assign,
      SET = EditAsterismsPatchInput(ADD = targetIds.assign)
    )
    UpdateAsterismsMutation[F].execute(input).void

  def removeTargetsFromAsterisms[F[_]: Async](
    programId: Program.Id,
    obsIds:    List[Observation.Id],
    targetIds: List[Target.Id]
  )(using FetchClient[F, ?, ObservationDB]) =
    val input = UpdateAsterismsInput(
      programId = programId,
      WHERE = obsIds.toWhereObservation.assign,
      SET = EditAsterismsPatchInput(DELETE = targetIds.assign)
    )
    UpdateAsterismsMutation[F].execute(input).void
