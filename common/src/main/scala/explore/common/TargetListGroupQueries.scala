// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Order
import cats.effect.Async
import cats.effect.IO
import cats.implicits._
import clue.TransactionalClient
import clue.data.syntax._
import crystal.react.reuse._
import explore.AppCtx
import explore.components.graphql.LiveQueryRenderMod
import explore.implicits._
import explore.model.ObsSummaryWithConstraints
import explore.model.TargetListGroup
import explore.schemas.implicits._
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

import TargetListGroupQueriesGQL._

object TargetListGroupQueries {
  // The default cats ordering for sorted set sorts by size first, then contents. That's not what we want.
  // This is used for sorting the TargetListGroupObsList. If we change to sort by name or something
  // else, we can remove this.
  implicit val orderSortedSet: Order[SortedSet[Observation.Id]] = Order.by(_.toList)

  type ObservationResult = TargetListGroupObsQuery.Data.Observations.Nodes
  val ObservationResult = TargetListGroupObsQuery.Data.Observations.Nodes

  type TargetListGroupList = SortedMap[SortedSet[Observation.Id], TargetListGroup]
  type ObsList             = SortedMap[Observation.Id, ObsSummaryWithConstraints]

  case class TargetListGroupWithObs(
    targetListGroups: TargetListGroupList,
    observations:     ObsList
  )

  object TargetListGroupWithObs {
    val targetListGroups = Focus[TargetListGroupWithObs](_.targetListGroups)
    val observations     = Focus[TargetListGroupWithObs](_.observations)
  }

  implicit val targetListGroupWithObservationsReuse: Reusability[TargetListGroupWithObs] =
    Reusability.derive

  private def obsResultToSummary(obsR: ObservationResult): ObsSummaryWithConstraints =
    ObsSummaryWithConstraints(obsR.id,
                              obsR.constraintSet,
                              obsR.status,
                              obsR.activeStatus,
                              obsR.plannedTime.execution,
                              obsR.targets.id
    )

  private val queryToTargetListGroupWithObsGetter
    : Getter[TargetListGroupObsQuery.Data, TargetListGroupWithObs] = data =>
    TargetListGroupWithObs(
      data.scienceTargetListGroup.toSortedMap(TargetListGroup.obsIds.get),
      data.observations.nodes.map(obsResultToSummary).toSortedMap(ObsSummaryWithConstraints.id.get)
    )

  implicit class TargetListGroupObsQueryDataOps(val self: TargetListGroupObsQuery.Data.type)
      extends AnyVal {
    def asTargetListGroupWithObs = queryToTargetListGroupWithObsGetter
  }

  val TargetListGroupLiveQuery =
    ScalaFnComponent[View[TargetListGroupWithObs] ==> VdomNode](render =>
      AppCtx.using { implicit appCtx =>
        LiveQueryRenderMod[ObservationDB, TargetListGroupObsQuery.Data, TargetListGroupWithObs](
          TargetListGroupObsQuery.query().reuseAlways,
          (TargetListGroupObsQuery.Data.asTargetListGroupWithObs.get _).reuseAlways,
          List(TargetEnvQueriesGQL.ProgramTargetEnvEditSubscription.subscribe[IO]()).reuseAlways
        )(potRender(render))
      }
    )

  def replaceObservationScienceTargetList[F[_]: Async](
    obsId:      Observation.Id,
    targets:    List[Target]
  )(implicit c: TransactionalClient[F, ObservationDB]) = {
    val input = BulkReplaceTargetListInput(
      select = SelectTargetEnvironmentInput(observations = List(obsId).assign),
      replace = targets.map(_.toCreateInput)
    )
    ReplaceScienceTargetListMutation.execute[F](input).void
  }
}
