// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Order
import cats.effect.IO
import cats.implicits._
import crystal.react.ReuseView
import crystal.react.reuse._
import explore.components.LiveQuery
import explore.implicits._
import explore.model.ConstraintGroup
import explore.model.ObsIdSet
import explore.model.ObsSummaryWithTitleAndConf
import japgolly.scalajs.react._
import japgolly.scalajs.react.callback.CallbackCatsEffect._
import japgolly.scalajs.react.vdom.VdomNode
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.ui.reusability._
import monocle.Focus
import monocle.Getter
import queries.common.ConstraintGroupQueriesGQL._
import queries.common.ObsQueriesGQL
import react.common.ReactFnProps

import scala.collection.immutable.SortedMap

object ConstraintGroupQueries {
  // The default cats ordering for sorted set sorts by size first, then contents. That's not what we want.
  implicit val orderSortedSet: Order[ObsIdSet] = Order.by(_.toList)

  type ObservationResult = ConstraintGroupObsQuery.Data.Observations.Nodes
  val ObservationResult = ConstraintGroupObsQuery.Data.Observations.Nodes

  type ConstraintGroupList = SortedMap[ObsIdSet, ConstraintGroup]
  type ObsList             = SortedMap[Observation.Id, ObsSummaryWithTitleAndConf]

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

  private def obsResultToSummary(obsR: ObservationResult): ObsSummaryWithTitleAndConf =
    ObsSummaryWithTitleAndConf(
      obsR.id,
      obsR.title,
      obsR.subtitle,
      obsR.status,
      obsR.activeStatus,
      obsR.plannedTime.execution,
      obsR.scienceMode
    )

  private val queryToConstraintsWithObsGetter
    : Getter[ConstraintGroupObsQuery.Data, ConstraintSummaryWithObervations] =
    data =>
      ConstraintSummaryWithObervations(
        data.constraintSetGroup.nodes
          .toSortedMap(ConstraintGroup.obsIds.get),
        data.observations.nodes
          .map(obsResultToSummary)
          .toSortedMap(ObsSummaryWithTitleAndConf.id.get)
      )

  implicit class ConstraintGroupObsQueryDataOps(val self: ConstraintGroupObsQuery.Data.type)
      extends AnyVal {
    def asConstraintSummWithObs = queryToConstraintsWithObsGetter
  }

  final case class ConstraintGroupLiveQuery(
    programId:        Program.Id,
    render:           ReuseView[ConstraintSummaryWithObervations] ==> VdomNode
  )(implicit val ctx: AppContextIO)
      extends ReactFnProps[ConstraintGroupLiveQuery](ConstraintGroupLiveQuery.component)

  object ConstraintGroupLiveQuery {
    type Props = ConstraintGroupLiveQuery

    implicit val reuseProps: Reusability[Props] = Reusability.derive

    protected val component =
      ScalaFnComponent.withReuse[Props] { props =>
        implicit val ctx = props.ctx

        LiveQuery(
          ConstraintGroupObsQuery
            .query(props.programId)
            .map(ConstraintGroupObsQuery.Data.asConstraintSummWithObs.get)
            .reRunOnResourceSignals(
              ObsQueriesGQL.ProgramObservationsEditSubscription.subscribe[IO](props.programId)
            )
        )(props.render)
      }
  }
}
