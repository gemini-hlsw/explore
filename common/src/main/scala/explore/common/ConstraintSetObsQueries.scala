// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.effect.IO
import clue.data.Input
import eu.timepit.refined.types.string.NonEmptyString
import explore.AppCtx
import explore.components.graphql.LiveQueryRenderMod
import explore.data.KeyedIndexedList
import explore.implicits._
import explore.model.AirMassRange
import explore.model.ObsSummaryWithPointingAndConstraints
import explore.model.Pointing
import explore.model.reusability._
import explore.schemas.ObservationDB
import explore.schemas.ObservationDB.Types._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.CloudExtinction
import lucuma.core.enum.ImageQuality
import lucuma.core.enum.SkyBackground
import lucuma.core.enum.WaterVapor
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.ui.reusability._
import monocle.Getter
import monocle.macros.Lenses

import ConstraintSetObsQueriesGQL._

object ConstraintSetObsQueries {
  type ConstraintSetResult = ConstraintSetsObsQuery.Data.ConstraintSets.Nodes
  val ConstraintSetResult = ConstraintSetsObsQuery.Data.ConstraintSets.Nodes

  type PointingResult = ConstraintSetsObsQuery.Data.Observations.Nodes.ObservationTarget
  val PointingResult = ConstraintSetsObsQuery.Data.Observations.Nodes.ObservationTarget

  type ConstraintSetList = KeyedIndexedList[ConstraintSet.Id, ConstraintSetResult]
  type ObsList           = KeyedIndexedList[Observation.Id, ObsSummaryWithPointingAndConstraints]

  def defaultConstraintSetResult(name: NonEmptyString, id: ConstraintSet.Id): ConstraintSetResult =
    ConstraintSetResult(
      id = id,
      name = name,
      imageQuality = ImageQuality.PointEight,
      cloudExtinction = CloudExtinction.PointThree,
      skyBackground = SkyBackground.Gray,
      waterVapor = WaterVapor.Wet
    )

  def defaultCreateConstraintSet(cs: ConstraintSetResult): CreateConstraintSetInput =
    CreateConstraintSetInput(
      constraintSetId = Input(cs.id),
      programId = "p-2",
      name = cs.name,
      imageQuality = cs.imageQuality,
      cloudExtinction = cs.cloudExtinction,
      skyBackground = cs.skyBackground,
      waterVapor = cs.waterVapor,
      elevationRange = CreateElevationRangeInput(airmassRange =
        clue.data.Input(
          CreateAirmassRangeInput(min = AirMassRange.DefaultMin.value,
                                  max = AirMassRange.DefaultMax.value
          )
        )
      )
    )

  @Lenses
  case class ConstraintSetsWithObs(constraintSets: ConstraintSetList, obs: ObsList)

  private def convertPointing(
    pointing: PointingResult
  ): Pointing =
    pointing match {
      case PointingResult.Target(id, name)   => Pointing.PointingTarget(id, name)
      case PointingResult.Asterism(id, name) => Pointing.PointingAsterism(id, name, Nil)
    }

  private val constraintSetsObsQueryConstraintSetsWithObsGetter
    : Getter[ConstraintSetsObsQuery.Data, ConstraintSetsWithObs] = data => {
    ConstraintSetsWithObs(
      KeyedIndexedList.fromList(data.constraintSets.nodes, ConstraintSetResult.id.get),
      KeyedIndexedList.fromList(
        data.observations.nodes.map(node =>
          ObsSummaryWithPointingAndConstraints(node.id,
                                               node.observationTarget.map(convertPointing),
                                               node.constraintSet
          )
        ),
        ObsSummaryWithPointingAndConstraints.id.get
      )
    )
  }

  implicit class ConstraintSetsObsQueryDataOps(val self: ConstraintSetsObsQuery.Data.type)
      extends AnyVal {
    def asConstraintSetsWithObs = constraintSetsObsQueryConstraintSetsWithObsGetter
  }

  implicit val constraintSetWithObsReusability: Reusability[ConstraintSetsWithObs] =
    Reusability.derive

  val ConstraintSetObsLiveQuery =
    ScalaFnComponent[View[ConstraintSetsWithObs] ~=> VdomNode](render =>
      AppCtx.using { implicit appCtx =>
        LiveQueryRenderMod[ObservationDB, ConstraintSetsObsQuery.Data, ConstraintSetsWithObs](
          ConstraintSetsObsQuery.query(),
          ConstraintSetsObsQuery.Data.asConstraintSetsWithObs.get,
          List(
            ConstraintSetEditSubscription.subscribe[IO](),
            ObsQueriesGQL.ProgramObservationsEditSubscription.subscribe[IO]()
          )
        )(render)
      }
    )

}
