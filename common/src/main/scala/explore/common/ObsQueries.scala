// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.effect.IO
import explore.AppCtx
import explore.components.graphql.LiveQueryRenderMod
import explore.data.KeyedIndexedList
import explore.implicits._
import explore.model.ObsSummaryWithPointingAndConstraints
import explore.model.Pointing
import explore.model.reusability._
import explore.schemas.ObservationDB
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.ui.reuse._
import monocle.Getter

import ObsQueriesGQL._

object ObsQueries {

  type ObservationList = KeyedIndexedList[Observation.Id, ObsSummaryWithPointingAndConstraints]
  type ConstraintsInfo = List[ProgramObservationsQuery.Data.ConstraintSets.Nodes]

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
    : Getter[ProgramObservationsQuery.Data, (ConstraintsInfo, ObservationList)] = data => {
    val cs  = data.constraintSets.nodes
    val obs = KeyedIndexedList.fromList(
      data.observations.nodes.map(node =>
        ObsSummaryWithPointingAndConstraints(node.id,
                                             node.observationTarget.map(convertPointing),
                                             node.constraintSet
        )
      ),
      ObsSummaryWithPointingAndConstraints.id.get
    )
    (cs, obs)
  }

  implicit class ProgramObservationsQueryDataOps(val self: ProgramObservationsQuery.Data.type)
      extends AnyVal {
    def asObservationList = programObservationsQueryoObservationListGetter
  }

  val ObsLiveQuery =
    ScalaFnComponent[View[(ConstraintsInfo, ObservationList)] ==> VdomNode](render =>
      AppCtx.using { implicit appCtx =>
        LiveQueryRenderMod[ObservationDB,
                           ProgramObservationsQuery.Data,
                           (ConstraintsInfo, ObservationList)
        ](
          ProgramObservationsQuery.query(),
          ProgramObservationsQuery.Data.asObservationList.get,
          List(
            ProgramObservationsEditSubscription.subscribe[IO](),
            ConstraintSetObsQueriesGQL.ConstraintSetsEditSubscription.subscribe[IO]()
          )
        )(render)
      }
    )

}
