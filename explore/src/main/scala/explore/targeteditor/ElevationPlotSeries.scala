// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Eq
import cats.Order
import cats.data.NonEmptyMap
import cats.derived.*
import eu.timepit.refined.cats.given
import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.*
import lucuma.core.enums.Site
import lucuma.core.math.skycalc.SkyCalcResults
import lucuma.core.model.CoordinatesAtVizTime
import lucuma.core.model.ObjectTracking
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.util.NewType
import lucuma.typed.highcharts.mod.Point
import lucuma.typed.highcharts.mod.PointOptionsObject
import lucuma.ui.utils.unzip4

import java.time.Duration
import java.time.Instant
import scala.deriving.Mirror

import scalajs.js

@js.native
trait PointOptionsWithAirmass extends PointOptionsObject:
  var airmass: Double

@js.native
trait ElevationPointWithAirmass extends Point:
  var airmass: Double

// Can wrap data for a target or an asterism.
case class ElevationPlotSeries(
  name:     NonEmptyString,
  tracking: ObjectTracking,
  style:    ElevationPlotSeries.Style
) derives Eq:
  private val PlotEvery: Duration = Duration.ofMinutes(1)

  def pointsAtInstant(site: Site, start: Instant, end: Instant): ElevationPlotSeries.Points =
    ElevationPlotSeries.Points:
      SkyCalc.forInterval(
        site,
        start,
        end,
        PlotEvery,
        // We are computing the coordinates at each point in time, this may be expensive
        // and could be optimized for sidereals, but it's probably necessary for non-sidereals.
        tracking.at(_).map(_.value).getOrElse(tracking.baseCoordinates)
      )

object ElevationPlotSeries:
  object Id extends NewType[Either[Observation.Id, Target.Id]]:
    given Order[Id] = Order.by(_.value)
  type Id = Id.Type

  enum Style derives Eq:
    case Solid, Dashed

  given Reusability[ElevationPlotSeries] = Reusability.byEq

  inline private def setAirMass(
    x:     PointOptionsWithAirmass,
    value: Double
  ): PointOptionsWithAirmass =
    x.airmass = value
    x

  case class ChartData(
    targetAltitude:   List[PointOptionsWithAirmass],
    skyBrightness:    List[PointOptionsObject],
    parallacticAngle: List[PointOptionsObject],
    moonAltitude:     List[PointOptionsObject]
  )

  case class MoonData(moonPhase: Double, moonIllum: Double)

  case class Points(value: List[(Instant, SkyCalcResults)]):
    lazy val chartData: ChartData = {
      val series
        : List[(PointOptionsObject, PointOptionsObject, PointOptionsObject, PointOptionsObject)] =
        value.map: (instant, results) =>
          val millisSinceEpoch = instant.toEpochMilli.toDouble

          def point(value: Double): PointOptionsObject =
            PointOptionsObject()
              .setX(millisSinceEpoch)
              .setY(value)

          def pointWithAirmass(value: Double, airmass: Double): PointOptionsObject =
            setAirMass(
              point(value).asInstanceOf[PointOptionsWithAirmass],
              airmass
            )

          (pointWithAirmass(
             results.altitude.toAngle.toSignedDoubleDegrees,
             results.airmass
           ),
           point(results.totalSkyBrightness),
           point(results.parallacticAngle.toSignedDoubleDegrees),
           point(results.lunarElevation.toAngle.toSignedDoubleDegrees)
          )

      summon[Mirror.Of[ChartData]].fromProduct(series.unzip4)
    }

    lazy val moonData: MoonData =
      val (midOfNight, midOfNightResult) = value(value.length / 2)
      val moonPhase                      = MoonCalc.approxPhase(midOfNight)
      val moonIllum                      = midOfNightResult.lunarIlluminatedFraction.toDouble
      MoonData(moonPhase, moonIllum)

object ElevationPlotData extends NewType[NonEmptyMap[ElevationPlotSeries.Id, ElevationPlotSeries]]:
  given Reusability[ElevationPlotData] =
    Reusability.by[Type, Map[ElevationPlotSeries.Id, ElevationPlotSeries]](
      _.value.toSortedMap.unsorted
    )(using Reusability.map)
type ElevationPlotData = ElevationPlotData.Type
