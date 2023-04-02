// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Eq
import cats.Order
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

import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import explore.model.Focused.obsSet
import explore.model.TargetWithIdAndObs.targetWithObs
import explore.data.KeyedIndexedList
import lucuma.core.model.ConstraintSet

object AsterismQueries:
  // The default cats ordering for sorted set sorts by size first, then contents. That's not what we want.
  // This is used for sorting the AsterismGroupObsList. If we change to sort by name or something
  // else, we can remove this.
  given Order[ObsIdSet] = ObsIdSet.given_Order_ObsIdSet

  type AsterismIds = SortedSet[Target.Id]

  type AsterismGroupList   = SortedMap[ObsIdSet, AsterismIds]
  type TargetList          = SortedMap[Target.Id, Target]
  type TargetWithObsList   = SortedMap[Target.Id, TargetWithObs]
  // KeyedIndexedList is only useful is manual order is going to matter.
  // For the moment I'm keeping it because it seems it will matter at some point.
  // Otherwise, we should change to a SortedMap.
  type ObservationList     = KeyedIndexedList[Observation.Id, ObsSummary]
  type ConstraintGroupList = SortedMap[ObsIdSet, ConstraintSet]

  case class ProgramSummaries(
    // asterismGroups: AsterismGroupList,
    // targetsWithObs: TargetWithObsList,
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
        .map(_.copy(id = clonedId, scienceTargetIds = targetIds.toSet))

    def insertObs(obsSummary: ObsSummary): ProgramSummaries =
      // val newObservations   = observations.inserted(obsSummary.id, obsSummary, observations.length)
      // val newTargetsWithObs = obsSummary.scienceTargetIds.foldLeft(targetsWithObs)((twos, id) =>
      //   twos.updatedWith(id)(_.map(r => r.copy(obsIds = r.obsIds + obsSummary.id)))
      // )

      // val targetIds         = SortedSet.from(obsSummary.scienceTargetIds)
      // val newIdSet          = ObsIdSet.one(obsSummary.id)
      // val newAsterismGr     = AsterismGroup(newIdSet, targetIds)
      // val currentAsterismGr = asterismGroups.find((ids, grpIds) => grpIds === targetIds)
      // val newAsterismGroups =
      //   currentAsterismGr.fold(asterismGroups + newAsterismGr.asObsKeyValue)((ids, _) =>
      //     asterismGroups - ids + AsterismGroup(ids ++ newIdSet, targetIds).asObsKeyValue
      //   )

      // ProgramSummaries(newAsterismGroups, newTargetsWithObs, newObservations)
      ProgramSummaries.observations.modify(
        _.inserted(obsSummary.id, obsSummary, observations.length)
      )(this)

    def removeObsWithTargets(
      obsId:     Observation.Id,
      targetIds: SortedSet[Target.Id]
    ): ProgramSummaries =
      ProgramSummaries.observations.modify(_.removed(obsId))(this)

      // val newObservations   = observations.removed(obsId)
      // val newTargetsWithObs = targetIds.foldLeft(targetsWithObs)((twos, id) =>
      //   twos.updatedWith(id)(_.map(r => r.copy(obsIds = r.obsIds - obsId)))
      // )

      // val currentAsterismGr = asterismGroups.find((ids, grpIds) => grpIds === targetIds)
      // val newAsterismGroups = currentAsterismGr.fold(asterismGroups) { (currentIds, _) =>
      //   val remainingIds = currentIds.removeOne(obsId)
      //   val tmpGroups    = remainingIds.fold(asterismGroups)(remaining =>
      //     asterismGroups + AsterismGroup(remaining, targetIds).asObsKeyValue
      //   )
      //   tmpGroups - currentIds
      // }
      // ProgramSummaries(newAsterismGroups, newTargetsWithObs, newObservations)
  }

  object ProgramSummaries:
    // val asterismGroups = Focus[ProgramSummaries](_.asterismGroups)
    // val targetsWithObs = Focus[ProgramSummaries](_.targetsWithObs)
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
