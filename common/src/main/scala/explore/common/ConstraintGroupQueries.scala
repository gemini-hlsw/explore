// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Order
import cats.effect.IO
import cats.implicits._
import crystal.react.View
import crystal.react.reuse._
import explore.AppCtx
import explore.components.graphql.LiveQueryRenderMod
import explore.implicits._
import explore.model.ConstraintGroup
import explore.model.ObsIdSet
import explore.model.ObsSummaryWithTargetsAndConf
import explore.model.TargetSummary
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.VdomNode
import lucuma.core.model.Observation
import lucuma.schemas.ObservationDB
import lucuma.ui.reusability._
import monocle.Focus
import monocle.Getter

import scala.collection.immutable.SortedMap

import ConstraintGroupQueriesGQL._

object ConstraintGroupQueries {
  // The default cats ordering for sorted set sorts by size first, then contents. That's not what we want.
  implicit val orderSortedSet: Order[ObsIdSet] = Order.by(_.toList)

  type ObservationResult = ConstraintGroupObsQuery.Data.Observations.Nodes
  val ObservationResult = ConstraintGroupObsQuery.Data.Observations.Nodes

  private def convertTarget(
    target: ConstraintGroupObsQuery.Data.Observations.Nodes.TargetEnvironment.Asterism
  ): TargetSummary = TargetSummary(target.id, target.name, none)

  type ConstraintGroupList = SortedMap[ObsIdSet, ConstraintGroup]
  type ObsList             = SortedMap[Observation.Id, ObsSummaryWithTargetsAndConf]

  case class ConstraintSummaryWithObervations(
    constraintGroups: ConstraintGroupList,
    observations:     ObsList
  )

  object ConstraintSummaryWithObervations {
    val constraintGroups = Focus[ConstraintSummaryWithObervations](_.constraintGroups)
    val observations     = Focus[ConstraintSummaryWithObervations](_.observations)
  }

  implicit val constraintsSummWithObsReuse: Reusability[ConstraintSummaryWithObervations] =
    Reusability.derive

  private def obsResultToSummary(obsR: ObservationResult): ObsSummaryWithTargetsAndConf =
    ObsSummaryWithTargetsAndConf(obsR.id,
                                 obsR.targetEnvironment.asterism.map(convertTarget),
                                 obsR.status,
                                 obsR.activeStatus,
                                 obsR.plannedTime.execution
    )

  private val queryToConstraintsWithObsGetter
    : Getter[ConstraintGroupObsQuery.Data, ConstraintSummaryWithObervations] =
    data =>
      ConstraintSummaryWithObervations(
        data.constraintSetGroup.nodes
          .toSortedMap(ConstraintGroup.obsIds.get),
        data.observations.nodes
          .map(obsResultToSummary)
          .toSortedMap(ObsSummaryWithTargetsAndConf.id.get)
      )

  implicit class ConstraintGroupObsQueryDataOps(val self: ConstraintGroupObsQuery.Data.type)
      extends AnyVal {
    def asConstraintSummWithObs = queryToConstraintsWithObsGetter
  }

  val ConstraintGroupLiveQuery =
    ScalaFnComponent[Reuse[View[ConstraintSummaryWithObervations]] ==> VdomNode](render =>
      AppCtx.using { implicit appCtx =>
        LiveQueryRenderMod[ObservationDB,
                           ConstraintGroupObsQuery.Data,
                           ConstraintSummaryWithObervations
        ](
          ConstraintGroupObsQuery.query().reuseAlways,
          (ConstraintGroupObsQuery.Data.asConstraintSummWithObs.get _).reuseAlways,
          List(ObsQueriesGQL.ProgramObservationsEditSubscription.subscribe[IO]()).reuseAlways
        )(potRender(render))
      }
    )
}
