// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.data.NonEmptyList
import cats.effect.IO
import explore.AppCtx
import explore.components.graphql.LiveQueryRenderMod
import explore.data.KeyedIndexedList
import explore.implicits._
import explore.model.ConstraintsSummary
import explore.model.ObsSummaryWithPointingAndConstraints
import explore.model.Pointing
import explore.model.reusability._
import explore.schemas.ObservationDB
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import monocle.Getter

import ObsQueriesGQL._

object ObsQueries {

  type ObservationList = KeyedIndexedList[Observation.Id, ObsSummaryWithPointingAndConstraints]

  private def convertPointing(
    pointing: ProgramObservationsQuery.Data.Observations.Nodes.ObservationTarget
  ): Pointing =
    pointing match {
      case ProgramObservationsQuery.Data.Observations.Nodes.ObservationTarget.Target(id, name)   =>
        Pointing.PointingTarget(id, name)
      case ProgramObservationsQuery.Data.Observations.Nodes.ObservationTarget.Asterism(id, name) =>
        Pointing.PointingAsterism(id, name, Nil)
    }

  implicit class ConstraintSetsObsQueryDataOps(
    val self: ProgramObservationsQuery.Data.ConstraintSets.Nodes
  ) extends AnyVal {
    def asConstraintSummary: ConstraintsSummary =
      new ConstraintsSummary {
        val id              = self.id
        val name            = self.name
        val imageQuality    = self.imageQuality
        val cloudExtinction = self.cloudExtinction
        val skyBackground   = self.skyBackground
        val waterVapor      = self.waterVapor
      }
  }

  private val programObservationsQueryoObservationListGetter
    : Getter[ProgramObservationsQuery.Data, (List[ConstraintsSummary], ObservationList)] = data => {
    val cs  = data.constraintSets.nodes.map(_.asConstraintSummary)
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
    ScalaFnComponent[View[(List[ConstraintsSummary], ObservationList)] ~=> VdomNode](render =>
      AppCtx.using { implicit appCtx =>
        LiveQueryRenderMod[ObservationDB,
                           ProgramObservationsQuery.Data,
                           (List[ConstraintsSummary], ObservationList)
        ](
          ProgramObservationsQuery.query(),
          ProgramObservationsQuery.Data.asObservationList.get,
          NonEmptyList.of(
            ProgramObservationsEditSubscription.subscribe[IO](),
            ConstraintSetObsQueriesGQL.ConstraintSetEditSubscription.subscribe[IO]()
          )
        )(render)
      }
    )

}
