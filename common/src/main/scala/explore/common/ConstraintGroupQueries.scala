// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Order
import cats.effect.IO
import cats.implicits._
import crystal.react.reuse._
import explore.AppCtx
import explore.components.graphql.LiveQueryRenderMod
import explore.implicits._
import explore.model.ConstraintGroup
import explore.model.ObsSummaryWithPointingAndConf
import explore.model.Pointing
import explore.schemas.ObservationDB
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.VdomNode
import lucuma.core.model.Observation
import lucuma.ui.reusability._
import monocle.Focus
import monocle.Getter

import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet

import ConstraintGroupQueriesGQL._

object ConstraintGroupQueries {
  // The default cats ordering for sorted set sorts by size first, then contents. That's not what we want.
  implicit val orderSortedSet: Order[SortedSet[Observation.Id]] = Order.by(_.toList)

  type ConstraintGroupResult = ConstraintGroupObsQuery.Data.ConstraintSetGroup.Nodes
  val ConstraintGroupResult = ConstraintGroupObsQuery.Data.ConstraintSetGroup.Nodes

  type ObservationResult = ConstraintGroupObsQuery.Data.Observations.Nodes
  val ObservationResult = ConstraintGroupObsQuery.Data.Observations.Nodes

  private def convertPointing(
    pointing: ConstraintGroupObsQuery.Data.Observations.Nodes.ObservationTarget
  ): Pointing = pointing match {
    case ConstraintGroupObsQuery.Data.Observations.Nodes.ObservationTarget
          .Target(targetId, targetName) =>
      Pointing.PointingTarget(targetId, targetName)
    case ConstraintGroupObsQuery.Data.Observations.Nodes.ObservationTarget
          .Asterism(asterismId, asterismName) =>
      Pointing.PointingAsterism(asterismId, asterismName, Nil)
  }

  type ConstraintGroupList = SortedMap[SortedSet[Observation.Id], ConstraintGroup]
  type ObsList             = SortedMap[Observation.Id, ObsSummaryWithPointingAndConf]

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

  private def obsResultToSummary(obsR: ObservationResult): ObsSummaryWithPointingAndConf =
    ObsSummaryWithPointingAndConf(obsR.id,
                                  obsR.observationTarget.map(convertPointing),
                                  obsR.status,
                                  obsR.activeStatus,
                                  obsR.plannedTime.execution
    )

  private def toSortedMap[K: Ordering, A](list: List[A], getKey: A => K) =
    SortedMap.from(list.map(a => (getKey(a), a)))

  private val queryToConstraintsWithObsGetter
    : Getter[ConstraintGroupObsQuery.Data, ConstraintSummaryWithObervations] =
    data =>
      ConstraintSummaryWithObervations(
        toSortedMap(
          data.constraintSetGroup.nodes.map(_.asConstraintGroup),
          ConstraintGroup.obsIds.get
        ),
        toSortedMap(data.observations.nodes.map(obsResultToSummary),
                    ObsSummaryWithPointingAndConf.id.get
        )
      )

  implicit class ConstraintGroupObsQueryDataOps(val self: ConstraintGroupObsQuery.Data.type)
      extends AnyVal {
    def asConstraintSummWithObs = queryToConstraintsWithObsGetter
  }

  implicit class ConstraintGroupResultOps(val self: ConstraintGroupResult) extends AnyVal {
    def asConstraintGroup =
      ConstraintGroup(self.constraintSet, SortedSet.from(self.observations.nodes.map(_.id)))
  }

  val ConstraintGroupLiveQuery =
    ScalaFnComponent[View[ConstraintSummaryWithObervations] ==> VdomNode](render =>
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
