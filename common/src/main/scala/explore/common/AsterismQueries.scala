// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Order
import cats.effect.Async
import cats.implicits._
import clue.TransactionalClient
import clue.data.syntax._
import explore.implicits._
import explore.model.AsterismGroup
import explore.model.ObsIdSet
import explore.model.ObsSummaryWithConstraintsAndConf
import explore.model.TargetGroup
import japgolly.scalajs.react._
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types._
import lucuma.ui.reusability._
import monocle.Focus
import monocle.Getter
import queries.common.AsterismQueriesGQL._
import queries.common.ObsQueriesGQL._

import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

object AsterismQueries {
  // The default cats ordering for sorted set sorts by size first, then contents. That's not what we want.
  // This is used for sorting the AsterismGroupObsList. If we change to sort by name or something
  // else, we can remove this.
  implicit val orderSortedSet: Order[ObsIdSet] = ObsIdSet.orderObsIdSet

  type ObservationResult = AsterismGroupObsQuery.Data.Observations.Nodes
  val ObservationResult = AsterismGroupObsQuery.Data.Observations.Nodes

  type AsterismGroupList = SortedMap[ObsIdSet, AsterismGroup]
  type TargetGroupList   = SortedMap[Target.Id, TargetGroup]
  type ObsList           = SortedMap[Observation.Id, ObsSummaryWithConstraintsAndConf]

  case class AsterismGroupsWithObs(
    asterismGroups: AsterismGroupList,
    targetGroups:   TargetGroupList,
    observations:   ObsList
  )

  object AsterismGroupsWithObs {
    val asterismGroups = Focus[AsterismGroupsWithObs](_.asterismGroups)
    val targetGroups   = Focus[AsterismGroupsWithObs](_.targetGroups)
    val observations   = Focus[AsterismGroupsWithObs](_.observations)
  }

  // Some helper methods on AsterismGroupList
  implicit class AsterismGroupListOps(val self: AsterismGroupList) extends AnyVal {
    def findContainingObsIds(obsIds: ObsIdSet): Option[AsterismGroup] =
      self.find { case (ids, _) => obsIds.subsetOf(ids) }.map(_._2)

    def findWithTargetIds(targetIds: SortedSet[Target.Id]): Option[AsterismGroup] =
      self.find { case (_, ag) => ag.targetIds === targetIds }.map(_._2)
  }

  implicit val asterismGroupWithObsReuse: Reusability[AsterismGroupsWithObs] =
    Reusability.derive

  private def obsResultToSummary(obsR: ObservationResult): ObsSummaryWithConstraintsAndConf =
    ObsSummaryWithConstraintsAndConf(
      obsR.id,
      obsR.constraintSet,
      obsR.status,
      obsR.activeStatus,
      obsR.plannedTime.execution,
      obsR.targetEnvironment.asterism.map(_.id).toSet,
      obsR.scienceMode,
      obsR.visualizationTime,
      obsR.posAngleConstraint
    )

  private val queryToAsterismGroupWithObsGetter
    : Getter[AsterismGroupObsQuery.Data, AsterismGroupsWithObs] = data => {
    val asterismGroups = data.asterismGroup.nodes
      .map { node =>
        ObsIdSet.fromList(node.observationIds).map { obsIdSet =>
          AsterismGroup(obsIdSet, SortedSet.from(node.asterism.map(_.id)))
        }
      }
      .flatten
      .toSortedMap(_.obsIds)
    val targetGroups   = data.targetGroup.nodes.toSortedMap(_.targetWithId.id)
    AsterismGroupsWithObs(
      asterismGroups,
      targetGroups,
      data.observations.nodes
        .map(obsResultToSummary)
        .toSortedMap(ObsSummaryWithConstraintsAndConf.id.get)
    )
  }

  implicit class AsterismGroupObsQueryDataOps(val self: AsterismGroupObsQuery.Data.type)
      extends AnyVal {
    def asAsterismGroupWithObs = queryToAsterismGroupWithObsGetter
  }

  def replaceAsterism[F[_]: Async](
    obsIds:     List[Observation.Id],
    targetIds:  List[Target.Id]
  )(implicit c: TransactionalClient[F, ObservationDB]) = {
    val input = EditObservationsInput(
      select = ObservationSelectInput(observationIds = obsIds.assign),
      patch = ObservationPropertiesInput(
        targetEnvironment = TargetEnvironmentInput(asterism = targetIds.assign).assign
      )
    )
    EditObservationMutation.execute[F](input).void
  }

  def addTargetToAsterisms[F[_]: Async](
    obsIds:     List[Observation.Id],
    targetId:   Target.Id
  )(implicit c: TransactionalClient[F, ObservationDB]) = {
    val input = EditAsterismsInput(
      select = ObservationSelectInput(observationIds = obsIds.assign),
      patch = List(EditAsterismsPatchInput(add = targetId.assign))
    )
    EditAsterismsMutation.execute[F](input).void
  }

  def removeTargetFromAsterisms[F[_]: Async](
    obsIds:     List[Observation.Id],
    targetId:   Target.Id
  )(implicit c: TransactionalClient[F, ObservationDB]) = {
    val input = EditAsterismsInput(
      select = ObservationSelectInput(observationIds = obsIds.assign),
      patch = List(EditAsterismsPatchInput(delete = targetId.assign))
    )
    EditAsterismsMutation.execute[F](input).void
  }
}
