// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.Eq
import cats.Order
import cats.effect.IO
import cats.implicits._
import crystal.react.reuse._
import explore.AppCtx
import explore.components.graphql.LiveQueryRenderMod
import explore.implicits._
import explore.model.Pointing
import explore.model.ObsSummaryWithPointingAndConstraints
import explore.schemas.ObservationDB
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.VdomNode
import lucuma.core.model.Observation
import lucuma.ui.reusability._
import monocle.Getter
import monocle.Focus
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

  type ConstraintSetData = ConstraintGroupObsQuery.Data.ConstraintSetGroup.Nodes.ConstraintSet

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

  case class ConstraintGroup(constraintSet: ConstraintSetData, obsIds: SortedSet[Observation.Id]) {
    def addObsId(obsId:    Observation.Id): ConstraintGroup =
      ConstraintGroup.obsIds.modify(_ + obsId)(this)
    def removeObsId(obsId: Observation.Id): ConstraintGroup =
      ConstraintGroup.obsIds.modify(_ - obsId)(this)
    def asKeyValue: (SortedSet[Observation.Id], ConstraintGroup) = (this.obsIds, this)
  }

  object ConstraintGroup {
    val constraintSet = Focus[ConstraintGroup](_.constraintSet)
    val obsIds        = Focus[ConstraintGroup](_.obsIds)

    def fromConstraintGroupResult(cgr: ConstraintGroupResult): ConstraintGroup =
      ConstraintGroup(cgr.constraintSet, SortedSet.from(cgr.observations.nodes.map(_.id)))
  }

  type ConstraintGroupList = SortedMap[SortedSet[Observation.Id], ConstraintGroup]
  // Don't really need the constraints in the obs summary, but will need to figure out what to display in the ObsBadge
  type ObsList             = SortedMap[Observation.Id, ObsSummaryWithPointingAndConstraints]

  case class ConstraintSummaryWithObervations(
    constraintGroups: ConstraintGroupList,
    observations:     ObsList
  )

  object ConstraintSummaryWithObervations {
    val constraintGroups = Focus[ConstraintSummaryWithObervations](_.constraintGroups)
    val observations     = Focus[ConstraintSummaryWithObervations](_.observations)
  }

  implicit val constraintGroupEq: Eq[ConstraintGroup]                                     = Eq.by(cg => (cg.constraintSet, cg.obsIds))
  implicit val constraintGroupReuse: Reusability[ConstraintGroup]                         = Reusability.derive
  implicit val constraintsSummWithObsReuse: Reusability[ConstraintSummaryWithObervations] =
    Reusability.derive

  private def obsResultToSummary(obsR: ObservationResult): ObsSummaryWithPointingAndConstraints =
    ObsSummaryWithPointingAndConstraints(obsR.id,
                                         obsR.observationTarget.map(convertPointing),
                                         obsR.constraintSet,
                                         obsR.status,
                                         obsR.activeStatus,
                                         obsR.plannedTime.execution
    )

  private def toSortedMap[K: Ordering, A](list: List[A], getKey: A => K) =
    SortedMap.from(list.map(a => (getKey(a), a)))

  private val queryToConstraintsWithObsGetter
    : Getter[ConstraintGroupObsQuery.Data, ConstraintSummaryWithObervations] =
    data => {
      ConstraintSummaryWithObervations(
        toSortedMap(
          data.constraintSetGroup.nodes.map(ConstraintGroup.fromConstraintGroupResult(_)),
          ConstraintGroup.obsIds.get
        ),
        toSortedMap(data.observations.nodes.map(obsResultToSummary),
                    ObsSummaryWithPointingAndConstraints.id.get
        )
      )
    }

  implicit class ConstraintGroupObsQueryDataOps(val self: ConstraintGroupObsQuery.Data.type)
      extends AnyVal {
    def asConstraintSummWithObs = queryToConstraintsWithObsGetter
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
