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
import explore.model.ObsSummaryWithConstraintsAndConf
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

object AsterismQueries:
  // The default cats ordering for sorted set sorts by size first, then contents. That's not what we want.
  // This is used for sorting the AsterismGroupObsList. If we change to sort by name or something
  // else, we can remove this.
  given Order[ObsIdSet] = ObsIdSet.given_Order_ObsIdSet

  type ObservationResult = AsterismGroupObsQuery.Data.Observations.Matches
  val ObservationResult = AsterismGroupObsQuery.Data.Observations.Matches

  type AsterismGroupList = SortedMap[ObsIdSet, AsterismGroup]
  type TargetWithObsList = SortedMap[Target.Id, TargetWithObs]
  type ObsList           = SortedMap[Observation.Id, ObsSummaryWithConstraintsAndConf]

  case class AsterismGroupsWithObs(
    asterismGroups: AsterismGroupList,
    targetsWithObs: TargetWithObsList,
    observations:   ObsList
  ) {
    def cloneObsWithTargets(
      originalId: Observation.Id,
      clonedId:   Observation.Id,
      targetIds:  List[Target.Id]
    ): Option[ObsSummaryWithConstraintsAndConf] =
      observations.get(originalId).map(_.copy(id = clonedId, scienceTargetIds = targetIds.toSet))

    def insertObs(obsSummary: ObsSummaryWithConstraintsAndConf): AsterismGroupsWithObs = {
      val newObservations   = observations + (obsSummary.id -> obsSummary)
      val newTargetsWithObs = obsSummary.scienceTargetIds.foldLeft(targetsWithObs)((twos, id) =>
        twos.updatedWith(id)(_.map(r => r.copy(obsIds = r.obsIds + obsSummary.id)))
      )

      val targetIds         = SortedSet.from(obsSummary.scienceTargetIds)
      val newIdSet          = ObsIdSet.one(obsSummary.id)
      val newAsterismGr     = AsterismGroup(newIdSet, targetIds)
      val currentAsterismGr = asterismGroups.find((ids, grp) => grp.targetIds === targetIds)
      val newAsterismGroups =
        currentAsterismGr.fold(asterismGroups + newAsterismGr.asObsKeyValue)((ids, _) =>
          asterismGroups - ids + AsterismGroup(ids ++ newIdSet, targetIds).asObsKeyValue
        )

      AsterismGroupsWithObs(newAsterismGroups, newTargetsWithObs, newObservations)
    }

    def removeObsWithTargets(
      obsId:     Observation.Id,
      targetIds: SortedSet[Target.Id]
    ): AsterismGroupsWithObs = {
      val newObservations   = observations - obsId
      val newTargetsWithObs = targetIds.foldLeft(targetsWithObs)((twos, id) =>
        twos.updatedWith(id)(_.map(r => r.copy(obsIds = r.obsIds - obsId)))
      )

      val currentAsterismGr = asterismGroups.find((ids, grp) => grp.targetIds === targetIds)
      val newAsterismGroups = currentAsterismGr.fold(asterismGroups) { (currentIds, _) =>
        val remainingIds = currentIds.removeOne(obsId)
        val tmpGroups    = remainingIds.fold(asterismGroups)(remaining =>
          asterismGroups + AsterismGroup(remaining, targetIds).asObsKeyValue
        )
        tmpGroups - currentIds
      }
      AsterismGroupsWithObs(newAsterismGroups, newTargetsWithObs, newObservations)
    }
  }

  object AsterismGroupsWithObs {
    val asterismGroups = Focus[AsterismGroupsWithObs](_.asterismGroups)
    val targetsWithObs = Focus[AsterismGroupsWithObs](_.targetsWithObs)
    val observations   = Focus[AsterismGroupsWithObs](_.observations)
  }

  // Some helper methods on AsterismGroupList
  extension (self: AsterismGroupList)
    def findContainingObsIds(obsIds: ObsIdSet): Option[AsterismGroup] =
      self.find { case (ids, _) => obsIds.subsetOf(ids) }.map(_._2)

    def findWithTargetIds(targetIds: SortedSet[Target.Id]): Option[AsterismGroup] =
      self.find { case (_, ag) => ag.targetIds === targetIds }.map(_._2)

  private def obsResultToSummary(obsR: ObservationResult): ObsSummaryWithConstraintsAndConf =
    ObsSummaryWithConstraintsAndConf(
      obsR.id,
      obsR.constraintSet,
      obsR.status,
      obsR.activeStatus,
      obsR.plannedTime.execution,
      obsR.targetEnvironment.asterism.map(_.id).toSet,
      obsR.observingMode,
      obsR.visualizationTime.map(_.toInstant),
      obsR.posAngleConstraint.some,
      obsR.scienceRequirements.spectroscopy.wavelength
    )

  private val queryToAsterismGroupWithObsGetter
    : Getter[AsterismGroupObsQuery.Data, AsterismGroupsWithObs] = data =>
    val asterismGroups = data.asterismGroup.matches
      .map { mtch =>
        ObsIdSet.fromList(mtch.observations.matches.map(_.id)).map { obsIdSet =>
          AsterismGroup(obsIdSet, SortedSet.from(mtch.asterism.map(_.id)))
        }
      }
      .flatten
      .toSortedMap(_.obsIds)

    val targetsWithObs = data.targetGroup.matches.toSortedMap(_.id, _.targetWithObs)

    AsterismGroupsWithObs(
      asterismGroups,
      targetsWithObs,
      data.observations.matches
        .map(obsResultToSummary)
        .toSortedMap(ObsSummaryWithConstraintsAndConf.id.get)
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
