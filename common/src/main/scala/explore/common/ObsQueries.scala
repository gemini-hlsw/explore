// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.effect.IO
import crystal.react.reuse._
import explore.AppCtx
import explore.components.graphql.LiveQueryRenderMod
import explore.data.KeyedIndexedList
import explore.implicits._
import explore.model.ObsSummaryWithPointingAndConstraints
import explore.model.Pointing
import explore.model.reusability._
import explore.schemas.ObservationDB
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import monocle.Getter

import ObsQueriesGQL._

object ObsQueries {

  type ObservationList = KeyedIndexedList[Observation.Id, ObsSummaryWithPointingAndConstraints]

  type ObservationData = ObsEditQuery.Data.Observation
  val ObservationData = ObsEditQuery.Data.Observation
  type ConstraintSetData = ObservationData.ConstraintSet
  val ConstraintSetData = ObservationData.ConstraintSet

  private def convertPointing(
    pointing: ProgramObservationsQuery.Data.Observations.Nodes.ObservationTarget
  ): Pointing =
    pointing match {
      case ProgramObservationsQuery.Data.Observations.Nodes.ObservationTarget.Target(id, name)   =>
        Pointing.PointingTarget(id, name)
      case ProgramObservationsQuery.Data.Observations.Nodes.ObservationTarget.Asterism(id, name) =>
        Pointing.PointingAsterism(id, name, Nil)
    }

  private val programObservationsQueryoObservationListGetter
    : Getter[ProgramObservationsQuery.Data, ObservationList] = data =>
    KeyedIndexedList.fromList(
      data.observations.nodes.map(node =>
        ObsSummaryWithPointingAndConstraints(node.id,
                                             node.observationTarget.map(convertPointing),
                                             node.constraintSet,
                                             node.status,
                                             node.activeStatus,
                                             node.plannedTime.execution
        )
      ),
      ObsSummaryWithPointingAndConstraints.id.get
    )

  implicit class ProgramObservationsQueryDataOps(val self: ProgramObservationsQuery.Data.type)
      extends AnyVal {
    def asObservationList = programObservationsQueryoObservationListGetter
  }

  val ObsLiveQuery =
    ScalaFnComponent[View[ObservationList] ==> VdomNode](render =>
      AppCtx.using { implicit appCtx =>
        LiveQueryRenderMod[ObservationDB, ProgramObservationsQuery.Data, ObservationList](
          ProgramObservationsQuery.query().reuseAlways,
          (ProgramObservationsQuery.Data.asObservationList.get _).reuseAlways,
          List(
            ProgramObservationsEditSubscription.subscribe[IO]()
          ).reuseAlways
        )(potRender(render))
      }
    )

}
