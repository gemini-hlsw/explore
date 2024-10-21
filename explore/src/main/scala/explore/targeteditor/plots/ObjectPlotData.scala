// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor.plots

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

import scalajs.js
import scalajs.js.JSConverters.*

@js.native
trait PointOptionsWithAirmass extends PointOptionsObject:
  var airmass: Double

@js.native
trait ElevationPointWithAirmass extends Point:
  var airmass: Double

// Can wrap data for a target or an asterism.
case class ObjectPlotData(
  name:     NonEmptyString,
  tracking: ObjectTracking,
  sites:    List[Site]
) derives Eq:
  private val PlotEvery: Duration = Duration.ofMinutes(1)

  def pointsAtInstant(site: Site, start: Instant, end: Instant): ObjectPlotData.Points =
    ObjectPlotData.Points:
      SkyCalc.forInterval(
        site,
        start,
        end,
        PlotEvery,
        // We are computing the coordinates at each point in time, this may be expensive
        // and could be optimized for sidereals, but it's probably necessary for non-sidereals.
        tracking.at(_).map(_.value).getOrElse(tracking.baseCoordinates)
      )

object ObjectPlotData:
  object Id extends NewType[Either[Observation.Id, Target.Id]]:
    given Order[Id] = Order.by(_.value)
  type Id = Id.Type

  given Reusability[ObjectPlotData] = Reusability.byEq

  inline private def setAirMass(
    x:     PointOptionsWithAirmass,
    value: Double
  ): PointOptionsWithAirmass =
    x.airmass = value
    x

  case class SeriesData(
    targetAltitude:   js.Array[PointOptionsWithAirmass],
    skyBrightness:    js.Array[PointOptionsObject],
    parallacticAngle: js.Array[PointOptionsObject],
    moonAltitude:     js.Array[PointOptionsObject]
  )

  object SeriesData:
    def apply(
      data: (
        List[PointOptionsWithAirmass],
        List[PointOptionsObject],
        List[PointOptionsObject],
        List[PointOptionsObject]
      )
    ): SeriesData =
      SeriesData(data._1.toJSArray, data._2.toJSArray, data._3.toJSArray, data._4.toJSArray)

  case class MoonData(moonPhase: Double, moonIllum: Double)

  case class Points(value: List[(Instant, SkyCalcResults)]):
    lazy val seriesData: SeriesData = {
      val series: List[
        (PointOptionsWithAirmass, PointOptionsObject, PointOptionsObject, PointOptionsObject)
      ] =
        value.map: (instant, results) =>
          val millisSinceEpoch = instant.toEpochMilli.toDouble

          def point(value: Double): PointOptionsObject =
            PointOptionsObject()
              .setX(millisSinceEpoch)
              .setY(value)

          def pointWithAirmass(value: Double, airmass: Double): PointOptionsWithAirmass =
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

      SeriesData(series.unzip4)
    }

    lazy val moonData: MoonData =
      val (midOfNight, midOfNightResult) = value(value.length / 2)
      val moonPhase                      = MoonCalc.approxPhase(midOfNight)
      val moonIllum                      = midOfNightResult.lunarIlluminatedFraction.toDouble
      MoonData(moonPhase, moonIllum)

object PlotData extends NewType[NonEmptyMap[ObjectPlotData.Id, ObjectPlotData]]:
  given Reusability[PlotData] =
    Reusability.by[Type, Map[ObjectPlotData.Id, ObjectPlotData]](
      _.value.toSortedMap.unsorted
    )(using Reusability.map)
type PlotData = PlotData.Type
