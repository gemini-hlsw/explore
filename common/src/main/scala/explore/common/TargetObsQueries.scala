// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.effect.IO
import crystal.react.reuse._
import eu.timepit.refined.types.string.NonEmptyString
import explore.AppCtx
import explore.components.graphql.LiveQueryRenderMod
import explore.data.KeyedIndexedList
import explore.implicits._
import explore.model.PointingId
import explore.model.reusability._
import explore.optics._
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Parallax
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.RightAscension
import lucuma.core.model.Asterism
import lucuma.core.model.Observation
import lucuma.core.model.SiderealTracking
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.ui.reusability._
import monocle.Focus
import monocle.Getter
import monocle.Iso
import monocle.Lens

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

  val baseCoordinatesRa: Lens[TargetResult, RightAscension] =
    TargetResult.tracking
      .andThen(SiderealTracking.baseCoordinates)
      .andThen(Coordinates.rightAscension)

  val baseCoordinatesDec: Lens[TargetResult, Declination] =
    TargetResult.tracking.andThen(SiderealTracking.baseCoordinates).andThen(Coordinates.declination)

  val pmRALens: Lens[TargetResult, Option[ProperMotion.RA]] =
    TargetResult.tracking.andThen(SiderealTracking.properMotion).andThen(unsafePMRALensO)

  val pmDecLens: Lens[TargetResult, Option[ProperMotion.Dec]] =
    TargetResult.tracking.andThen(SiderealTracking.properMotion).andThen(unsafePMDecLensO)

  val epoch: Lens[TargetResult, Epoch] =
    TargetResult.tracking.andThen(SiderealTracking.epoch)

  val pxLens: Lens[TargetResult, Option[Parallax]] =
    TargetResult.tracking.andThen(SiderealTracking.parallax)

  val rvLens: Lens[TargetResult, Option[RadialVelocity]] =
    TargetResult.tracking.andThen(SiderealTracking.radialVelocity)

  type TargetList         = KeyedIndexedList[Target.Id, TargetResult]
  type AsterismList       = KeyedIndexedList[Asterism.Id, AsterismIdName]
  type ObsList            = KeyedIndexedList[Observation.Id, ObsResult]
  type AsterismTargetList = KeyedIndexedList[Target.Id, AsterismResultTarget]

  case class AsterismIdName(
    id:      Asterism.Id,
    name:    Option[NonEmptyString],
    targets: AsterismTargetList
  )

  object AsterismIdName {
    val id      = Focus[AsterismIdName](_.id)
    val targets = Focus[AsterismIdName](_.targets)

    def fromAsterismResult(asterism: AsterismResult): AsterismIdName =
      AsterismIdName(
        asterism.id,
        asterism.name,
        KeyedIndexedList.fromList(asterism.targets.nodes, AsterismResultTarget.id.get)
      )
  }

  case class PointingsWithObs(
    targets:      TargetList,
    asterisms:    AsterismList,
    observations: ObsList
  )

  object PointingsWithObs {
    val targets      = Focus[PointingsWithObs](_.targets)
    val asterisms    = Focus[PointingsWithObs](_.asterisms)
    val observations = Focus[PointingsWithObs](_.observations)
  }

  val targetsObsQueryPointingId: Iso[ObsResult.Pointing, PointingId] =
    Iso[ObsResult.Pointing, PointingId] {
      case PointingTargetResult(id)   => PointingId.TargetId(id)
      case PointingAsterismResult(id) => PointingId.AsterismId(id)
    } {
      case PointingId.TargetId(id)   => PointingTargetResult(id)
      case PointingId.AsterismId(id) => PointingAsterismResult(id)
    }

  val targetsObsQueryObsPointingId: Lens[ObsResult, Option[PointingId]]                     =
    ObsResult.pointing.andThen(optionIso(targetsObsQueryPointingId))

  private val targetsObsQueryTargetsWithObs: Getter[TargetsObsQuery.Data, PointingsWithObs] =
    data =>
      PointingsWithObs(
        KeyedIndexedList.fromList(data.targets.nodes, _.id),
        KeyedIndexedList.fromList(data.asterisms.nodes.map(AsterismIdName.fromAsterismResult),
                                  AsterismIdName.id.get
        ),
        KeyedIndexedList.fromList(data.observations.nodes, ObsResult.id.get)
      )

  implicit class TargetsObsQueryDataOps(val self: TargetsObsQuery.Data.type) extends AnyVal {
    def asTargetsWithObs = targetsObsQueryTargetsWithObs
  }

  implicit val TargetResultReusability: Reusability[TargetResult] =
    Reusability.by(x => (x.id, x.name))
  implicit val AsterismIdNameReusability: Reusability[AsterismIdName]   =
    Reusability.by(x => (x.id, x.name, x.targets))

  implicit val targetsWithObsReusability: Reusability[PointingsWithObs] =
    Reusability.derive

  val TargetObsLiveQuery =
    ScalaFnComponent[View[PointingsWithObs] ==> VdomNode](render =>
      AppCtx.using { implicit appCtx =>
        LiveQueryRenderMod[ObservationDB, TargetsObsQuery.Data, PointingsWithObs](
          TargetsObsQuery.query().reuseAlways,
          (TargetsObsQuery.Data.asTargetsWithObs.get _).reuseAlways,
          List(
            TargetEditSubscription.subscribe[IO](),
            AsterismEditSubscription.subscribe[IO](),
            ObsQueriesGQL.ProgramObservationsEditSubscription.subscribe[IO]()
          ).reuseAlways
        )(potRender(render))
      }
    )
}
