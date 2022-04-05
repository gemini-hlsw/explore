// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Order
import cats.effect.Async
import cats.effect.IO
import cats.implicits._
import clue.TransactionalClient
import clue.data.syntax._
import crystal.react.ReuseView
import crystal.react.reuse._
import explore.AppCtx
import explore.components.graphql.LiveQueryRenderMod
import explore.implicits._
import explore.model.AsterismGroup
import explore.model.ObsIdSet
import explore.model.ObsSummaryWithConstraints
import explore.model.TargetGroup
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.VdomNode
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types._
import lucuma.ui.reusability._
import monocle.Focus
import monocle.Getter

import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

import queries.common.AsterismQueriesGQL._
import queries.common.ObsQueriesGQL
import queries.common.TargetQueriesGQL

object AsterismQueries {
  // The default cats ordering for sorted set sorts by size first, then contents. That's not what we want.
  // This is used for sorting the AsterismGroupObsList. If we change to sort by name or something
  // else, we can remove this.
  implicit val orderSortedSet: Order[ObsIdSet] = ObsIdSet.orderObsIdSet

  type ObservationResult = AsterismGroupObsQuery.Data.Observations.Nodes
  val ObservationResult = AsterismGroupObsQuery.Data.Observations.Nodes

  type AsterismGroupList = SortedMap[ObsIdSet, AsterismGroup]
  type TargetGroupList   = SortedMap[Target.Id, TargetGroup]
  type ObsList           = SortedMap[Observation.Id, ObsSummaryWithConstraints]

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

  private def obsResultToSummary(obsR: ObservationResult): ObsSummaryWithConstraints =
    ObsSummaryWithConstraints(obsR.id,
                              obsR.constraintSet,
                              obsR.status,
                              obsR.activeStatus,
                              obsR.plannedTime.execution,
                              obsR.targetEnvironment.asterism.map(_.id).toSet
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
      data.observations.nodes.map(obsResultToSummary).toSortedMap(ObsSummaryWithConstraints.id.get)
    )
  }

  implicit class AsterismGroupObsQueryDataOps(val self: AsterismGroupObsQuery.Data.type)
      extends AnyVal {
    def asAsterismGroupWithObs = queryToAsterismGroupWithObsGetter
  }

  val AsterismGroupLiveQuery =
    ScalaFnComponent[ReuseView[AsterismGroupsWithObs] ==> VdomNode](render =>
      AppCtx.using { implicit appCtx =>
        LiveQueryRenderMod[ObservationDB, AsterismGroupObsQuery.Data, AsterismGroupsWithObs](
          AsterismGroupObsQuery.query().reuseAlways,
          (AsterismGroupObsQuery.Data.asAsterismGroupWithObs.get _).reuseAlways,
          List(ObsQueriesGQL.ProgramObservationsEditSubscription.subscribe[IO](),
               TargetQueriesGQL.ProgramTargetEditSubscription.subscribe[IO]()
          ).reuseAlways
        )(potRender(render))
      }
    )

  def replaceAsterism[F[_]: Async](
    obsIds:     List[Observation.Id],
    targetIds:  List[Target.Id]
  )(implicit c: TransactionalClient[F, ObservationDB]) = {
    val input = BulkEditTargetEnvironmentInput(
      selectObservations = obsIds.assign,
      edit = TargetEnvironmentInput(asterism = targetIds.assign)
    )
    UpdateTargetEnvironmentMutation.execute[F](input).void
  }

  def addTargetToAsterisms[F[_]: Async](
    obsIds:     List[Observation.Id],
    targetId:   Target.Id
  )(implicit c: TransactionalClient[F, ObservationDB]) = {
    val input = BulkEditAsterismInput(selectObservations = obsIds.assign,
                                      edit = List(EditAsterismInput(add = targetId.assign))
    )
    UpdateAsterismMutation.execute[F](input).void
  }

  def removeTargetFromAsterisms[F[_]: Async](
    obsIds:     List[Observation.Id],
    targetId:   Target.Id
  )(implicit c: TransactionalClient[F, ObservationDB]) = {
    val input = BulkEditAsterismInput(selectObservations = obsIds.assign,
                                      edit = List(EditAsterismInput(delete = targetId.assign))
    )
    UpdateAsterismMutation.execute[F](input).void
  }
}
