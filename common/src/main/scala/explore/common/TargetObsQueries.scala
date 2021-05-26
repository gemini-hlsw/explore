// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.effect.IO
import eu.timepit.refined.types.string.NonEmptyString
import explore.AppCtx
import explore.components.graphql.LiveQueryRenderMod
import explore.data.KeyedIndexedList
import explore.implicits._
import explore.model.PointingId
import explore.model.reusability._
import explore.optics._
import explore.schemas.ObservationDB
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.RightAscension
import lucuma.core.model.Asterism
import lucuma.core.model.Observation
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.ui.reusability._
import monocle.Getter
import monocle.Iso
import monocle.Lens
import monocle.macros.Lenses

import TargetObsQueriesGQL._

object TargetObsQueries {

  type TargetResult = TargetsObsQuery.Data.Targets.Nodes
  val TargetResult = TargetsObsQuery.Data.Targets.Nodes

  type AsterismResult = TargetsObsQuery.Data.Asterisms.Nodes
  val AsterismResult = TargetsObsQuery.Data.Asterisms.Nodes

  type AsterismResultTarget = AsterismResult.Targets.Nodes
  val AsterismResultTarget = AsterismResult.Targets.Nodes

  type ObsResult = TargetsObsQuery.Data.Observations.Nodes
  val ObsResult = TargetsObsQuery.Data.Observations.Nodes

  type PointingTargetResult = ObsResult.Pointing.Target
  val PointingTargetResult = ObsResult.Pointing.Target
  type PointingAsterismResult = ObsResult.Pointing.Asterism
  val PointingAsterismResult = ObsResult.Pointing.Asterism

  /**
   * Lens for the base coordinates of TargetResult.Tracking
   */
  val baseCoordinates: Lens[TargetResult, Coordinates] =
    TargetResult.tracking ^|-> SiderealTracking.baseCoordinates

  val baseCoordinatesRa: Lens[TargetResult, RightAscension] =
    baseCoordinates ^|-> Coordinates.rightAscension

  val baseCoordinatesDec: Lens[TargetResult, Declination] =
    baseCoordinates ^|-> Coordinates.declination

  type TargetList         = KeyedIndexedList[Target.Id, TargetResult]
  type AsterismList       = KeyedIndexedList[Asterism.Id, AsterismIdName]
  type ObsList            = KeyedIndexedList[Observation.Id, ObsResult]
  type AsterismTargetList = KeyedIndexedList[Target.Id, AsterismResultTarget]

  @Lenses
  case class AsterismIdName(
    id:      Asterism.Id,
    name:    Option[NonEmptyString],
    targets: AsterismTargetList
  )
  object AsterismIdName {
    def fromAsterismResult(asterism: AsterismResult): AsterismIdName =
      AsterismIdName(
        asterism.id,
        asterism.name,
        KeyedIndexedList.fromList(asterism.targets.nodes, AsterismResultTarget.id.get)
      )
  }

  @Lenses
  case class PointingsWithObs(
    targets:      TargetList,
    asterisms:    AsterismList,
    observations: ObsList
  )

  val targetsObsQueryPointingId: Iso[ObsResult.Pointing, PointingId] =
    Iso[ObsResult.Pointing, PointingId] {
      case PointingTargetResult(id)   => PointingId.TargetId(id)
      case PointingAsterismResult(id) => PointingId.AsterismId(id)
    } {
      case PointingId.TargetId(id)   => PointingTargetResult(id)
      case PointingId.AsterismId(id) => PointingAsterismResult(id)
    }

  val targetsObsQueryObsPointingId: Lens[ObsResult, Option[PointingId]] =
    ObsResult.pointing.composeIso(optionIso(targetsObsQueryPointingId))

  private val targetsObsQueryTargetsWithObs: Getter[TargetsObsQuery.Data, PointingsWithObs] =
    data => {
      PointingsWithObs(
        KeyedIndexedList.fromList(data.targets.nodes, _.id),
        KeyedIndexedList.fromList(data.asterisms.nodes.map(AsterismIdName.fromAsterismResult),
                                  AsterismIdName.id.get
        ),
        KeyedIndexedList.fromList(data.observations.nodes, ObsResult.id.get)
      )
    }

  implicit class TargetsObsQueryDataOps(val self: TargetsObsQuery.Data.type) extends AnyVal {
    def asTargetsWithObs = targetsObsQueryTargetsWithObs
  }

  implicit val TargetResultReusability: Reusability[TargetResult]     =
    Reusability.by(x => (x.id, x.name))
  implicit val AsterismIdNameReusability: Reusability[AsterismIdName] =
    Reusability.by(x => (x.id, x.name, x.targets))

  implicit val targetsWithObsReusability: Reusability[PointingsWithObs] =
    Reusability.derive

  val TargetObsLiveQuery =
    ScalaFnComponent[View[PointingsWithObs] ~=> VdomNode](render =>
      AppCtx.using { implicit appCtx =>
        LiveQueryRenderMod[ObservationDB, TargetsObsQuery.Data, PointingsWithObs](
          TargetsObsQuery.query(),
          TargetsObsQuery.Data.asTargetsWithObs.get,
          List(
            TargetEditSubscription.subscribe[IO](),
            AsterismEditSubscription.subscribe[IO](),
            ObsQueriesGQL.ProgramObservationsEditSubscription.subscribe[IO](),
            ConstraintSetObsQueriesGQL.ConstraintSetsEditSubscription.subscribe[IO]()
          )
        )(render)
      }
    )
}
