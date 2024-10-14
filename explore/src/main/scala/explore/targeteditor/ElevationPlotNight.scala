// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Eq
import cats.derived.*
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import explore.*
import explore.highcharts.*
import explore.model.Constants
import explore.model.ElevationPlotOptions
import explore.model.enums.TimeDisplay
import explore.model.enums.Visible
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Site
import lucuma.core.enums.TwilightType
import lucuma.core.math.Angle
import lucuma.core.math.BoundedInterval
import lucuma.core.math.skycalc.ImprovedSkyCalc
import lucuma.core.model.ObservingNight
import lucuma.core.model.TwilightBoundedNight
import lucuma.core.util.time.*
import lucuma.react.common.ReactFnProps
import lucuma.react.highcharts.Chart
import lucuma.typed.highcharts.mod.*
import lucuma.typed.highcharts.mod.XAxisLabelsOptions
import lucuma.ui.components.MoonPhase
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import monocle.Lens
import org.typelevel.cats.time.given

import java.time.Duration
import java.time.Instant
import java.time.ZoneId
import java.time.ZoneOffset
import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import scala.collection.MapView
import scala.collection.immutable.HashSet
import scala.scalajs.js

import js.JSConverters.*

case class ElevationPlotNight(
  plotData:         ElevationPlotData,
  coordsTime:       Instant,
  // obsTime:          Option[Instant],
  excludeIntervals: List[BoundedInterval[Instant]],
  pendingTime:      Option[Duration],
  options:          View[ElevationPlotOptions]
) extends ReactFnProps(ElevationPlotNight.component):
  import ElevationPlotNight.ElevationSeries

  private def zoomFn(series: ElevationSeries): Lens[ElevationPlotOptions, Visible] =
    series match
      case ElevationSeries.Elevation        =>
        ElevationPlotOptions.elevationPlotElevationVisible
      case ElevationSeries.ParallacticAngle =>
        ElevationPlotOptions.elevationPlotParallacticAngleVisible
      case ElevationSeries.SkyBrightness    =>
        ElevationPlotOptions.elevationPlotSkyBrightnessVisible
      case ElevationSeries.LunarElevation   =>
        ElevationPlotOptions.elevationPlotLunarElevationVisible

  protected[ElevationPlotNight] def showSeriesCB(
    shownSeries: View[HashSet[ElevationSeries]],
    series:      ElevationSeries,
    chart:       Chart_
  ): Callback =
    options.zoom(zoomFn(series)).set(Visible.Shown) *>
      shownSeries.mod(_ + series) *>
      Callback:
        ElevationPlotNight.SkyBrightnessPercentileLines.foreach: line =>
          chart.yAxis(2).addPlotLine(line)
        ElevationPlotNight.SkyBrightnessPercentileBands
          .foreach: band =>
            chart.yAxis(2).addPlotBand(band)
      .when(series === ElevationSeries.SkyBrightness)
        .void

  protected[ElevationPlotNight] def hideSeriesCB(
    shownSeries: View[HashSet[ElevationSeries]],
    series:      ElevationSeries,
    chart:       Chart_
  ): Callback =
    options.zoom(zoomFn(series)).set(Visible.Hidden) *>
      shownSeries.mod(_ - series) *>
      Callback {
        ElevationPlotNight.SkyBrightnessPercentileLines
          .flatMap(_.id.toList)
          .foreach(id => chart.yAxis(2).removePlotLine(id))
        ElevationPlotNight.SkyBrightnessPercentileBands
          .flatMap(_.id.toList)
          .foreach(id => chart.yAxis(2).removePlotBand(id))
      }
        .when(series === ElevationSeries.SkyBrightness)
        .void

object ElevationPlotNight:
  private type Props = ElevationPlotNight

  private val MillisPerHour: Double = 60 * 60 * 1000

  private enum ElevationSeries(
    val name:    String,
    val yAxis:   Int,
    val data:    ElevationPlotSeries.ChartData => List[Chart.Data],
    val enabled: ElevationPlotOptions => Visible
  ) derives Eq:
    case Elevation
        extends ElevationSeries("Elevation", 0, _.targetAltitude, _.elevationPlotElevationVisible)
    case ParallacticAngle
        extends ElevationSeries(
          "Parallactic Angle",
          1,
          _.parallacticAngle,
          _.elevationPlotParallacticAngleVisible
        )
    case SkyBrightness
        extends ElevationSeries(
          "Sky Brightness",
          2,
          _.skyBrightness,
          _.elevationPlotSkyBrightnessVisible
        )
    case LunarElevation
        extends ElevationSeries(
          "Lunar Elevation",
          0,
          _.moonAltitude,
          _.elevationPlotLunarElevationVisible
        )

  private object ElevationSeries:
    given Reusability[ElevationSeries] = Reusability.byEq

  private def formatAngle(degs: Double): String =
    val dms     = Angle.DMS(Angle.fromDoubleDegrees(degs))
    val degrees = if (dms.degrees > 180) s"-${360 - dms.degrees}" else dms.degrees.toString
    val minutes = "%02d".format(dms.arcminutes)
    val seconds = "%02d".format(dms.arcseconds)
    s"$degrees°$minutes′$seconds″"

  private val SkyBrightnessPercentileLines =
    def plotLine(id: String, value: Double) =
      YAxisPlotLinesOptions()
        .setId(s"sky-brightness-$id")
        .setValue(value)
        .setClassName("plot-sky-brightness-percentile")
        .setDashStyle(DashStyleValue.Dot)
        .setZIndex(1)

    List(
      plotLine("20", 21.37),
      plotLine("50", 20.78),
      plotLine("80", 19.61)
    )

  private val SkyBrightnessPercentileBands: List[YAxisPlotBandsOptions] =
    def plotBand(id: String, label: String, from: Double, to: Double) =
      YAxisPlotBandsOptions()
        .setId(s"sky-brightness-$id")
        .setLabel(
          YAxisPlotBandsLabelOptions()
            .setText(s"<span class='plot-sky-brightness-band-label'>$label</span>")
            .setVerticalAlign(VerticalAlignValue.middle)
            .setAlign(AlignValue.right)
        )
        .setFrom(from)
        .setTo(to)
        .setClassName("plot-sky-brightness-band")
        .setZIndex(1)

    List(
      plotBand("darketst", "Darkest", 21.37, 22),
      plotBand("dark", "Dark", 20.78, 21.37),
      plotBand("gray", "Gray", 19.61, 20.78),
      plotBand("bright", "Bright", 17, 19.61)
    )

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateView(HashSet.from(ElevationSeries.values)) // shownSeries
      .useMemoBy((props, shownSeries) =>
        (props.options.get,
         props.plotData,
         //  props.obsTime,
         props.pendingTime,
         props.excludeIntervals,
         shownSeries.get
        )
      ): (props, shownSeries) =>
        (opts, plotData, pendingTime, excludeIntervals, _) =>
          val site: Site               = opts.site
          val timeDisplay: TimeDisplay = opts.timeDisplay

          val observingNight: ObservingNight        =
            ObservingNight.fromSiteAndLocalDate(site, opts.date)
          val tbOfficialNight: TwilightBoundedNight =
            observingNight.twilightBoundedUnsafe(TwilightType.Official)
          val tbNauticalNight: TwilightBoundedNight =
            observingNight.twilightBoundedUnsafe(TwilightType.Nautical)

          val start: Instant = tbOfficialNight.start
          val end: Instant   = tbOfficialNight.end

          def timezoneInstantFormat(instant: Instant, zoneId: ZoneId): String =
            ZonedDateTime
              .ofInstant(instant, zoneId)
              .roundTo(ChronoUnit.MINUTES)
              .format(Constants.GppTimeFormatter)

          def instantFormat(instant: Instant): String =
            timeDisplay match
              case TimeDisplay.Site     => timezoneInstantFormat(instant, site.timezone)
              case TimeDisplay.UT       => timezoneInstantFormat(instant, ZoneOffset.UTC)
              case TimeDisplay.Sidereal =>
                val skycalc = ImprovedSkyCalc(site.place)
                val sid     = {
                  val sid = skycalc.getSiderealTime(instant)
                  if (sid < 0) sid + 24 else sid
                }
                val hours   = sid.toInt
                val minutes = Math.round((sid % 1) * 60).toInt
                f"$hours%02d:$minutes%02d"

          def timeFormat(value: Double): String =
            instantFormat(Instant.ofEpochMilli(value.toLong))

          val timeDisplayStr: String =
            timeDisplay match
              case TimeDisplay.Site     => site.timezone.getId
              case TimeDisplay.UT       => "UTC"
              case TimeDisplay.Sidereal => "Site Sidereal"

          val tickFormatter: AxisLabelsFormatterCallbackFunction =
            (labelValue: AxisLabelsFormatterContextObject, _: AxisLabelsFormatterContextObject) =>
              timeFormat(labelValue.value.asInstanceOf[Double])

          val tooltipFormatter: TooltipFormatterCallbackFunction =
            (ctx: TooltipFormatterContextObject, _: Tooltip) =>
              val x                = ctx.x match
                case x: Double => x
                case x: String => x.toDouble
                case _         => 0.0
              val y                = ctx.y.asInstanceOf[js.UndefOr[String | Double]] match
                case y: Double => y
                case y: String => y.toDouble
                case _         => 0.0
              val time             = timeFormat(x)
              // HACK. TODO Think of something better
              val seriesIndex: Int = ctx.series.index.toInt / props.plotData.value.size.toInt
              val value            = seriesIndex match
                case 0 =>                      // Target elevation with airmass
                  formatAngle(y) + s"<br/>Airmass: ${"%.3f".format(ctx.point.asInstanceOf[ElevationPointWithAirmass].airmass)}"
                case 2 => "%.2f".format(ctx.y) // Sky Brightness
                case _ => formatAngle(y)       // Other elevations

              s"<strong>$time ($timeDisplayStr)</strong><br/>${ctx.series.name}: $value"

          val dusk: String = instantFormat(tbNauticalNight.start)
          val dawn: String = instantFormat(tbNauticalNight.end)

          val seriesData: MapView[ElevationPlotSeries.Id, ElevationPlotSeries.Points] =
            plotData.value.toSortedMap.view.mapValues(_.pointsAtInstant(site, start, end))

          val targetsBelowHorizonStr: Option[String] =
            Option.when(
              seriesData.forall: (_, targetSeriesData) =>
                ElevationSeries.Elevation
                  .data(targetSeriesData.chartData)
                  .forall(_.asInstanceOf[PointOptionsObject].y.forall(_.asInstanceOf[Double] <= 0))
            ):
              if (plotData.value.size === 1) "Target is below horizon"
              else "All targets are below horizon"

          val options: Options = Options()
            .setChart(commonOptions)
            .setTitle:
              TitleOptions().setText:
                s"Sunset ${observingNight.toLocalDate.minusDays(1)} ➟ Sunrise ${observingNight.toLocalDate} "
            .setCredits(CreditsOptions().setEnabled(false))
            .setTooltip(TooltipOptions().setFormatter(tooltipFormatter))
            .setXAxis(
              XAxisOptions()
                .setType(AxisTypeValue.datetime)
                .setLabels(XAxisLabelsOptions().setFormatter(tickFormatter))
                .setTickInterval(MillisPerHour)
                .setMinorTickInterval(MillisPerHour / 2)
                .setTitle:
                  targetsBelowHorizonStr.fold(XAxisTitleOptions()):
                    XAxisTitleOptions().setText(_)
                .setPlotBands(
                  (excludeIntervals.map(window =>
                    XAxisPlotBandsOptions()
                      .setFrom(window.lower.toEpochMilli.toDouble)
                      .setTo(window.upper.toEpochMilli.toDouble)
                      .setClassName("plot-band-exclude-window")
                  ) ++
                    List(
                      XAxisPlotBandsOptions()
                        .setFrom(tbNauticalNight.start.toEpochMilli.toDouble)
                        .setTo(tbNauticalNight.end.toEpochMilli.toDouble)
                        .setClassName("plot-band-twilight-nautical")
                        // We need z-index > 0 to display over grid. But not too high, or it will display over tooltips.
                        .setZIndex(1)
                        .setLabel(
                          XAxisPlotBandsLabelOptions()
                            .setText(s"  Evening 12° - Twilight: $dusk")
                            .setRotation(270)
                            .setAlign(AlignValue.left)
                            .setTextAlign(AlignValue.center)
                            .setVerticalAlign(VerticalAlignValue.middle)
                        ),
                      XAxisPlotBandsOptions() // Empty bands don't work on highcharts 11.4.8. Instead we create the same band in revese and no fill
                        .setFrom(tbNauticalNight.end.toEpochMilli.toDouble)
                        .setTo(tbNauticalNight.start.toEpochMilli.toDouble)
                        .setClassName("plot-band-twilight-nautical-end")
                        .setZIndex(1)
                        .setLabel(
                          XAxisPlotBandsLabelOptions()
                            .setText(s"  Morning 12° - Twilight: $dawn")
                            .setRotation(270)
                            .setAlign(AlignValue.right)
                            .setTextAlign(AlignValue.center)
                            .setVerticalAlign(VerticalAlignValue.middle)
                        )
                    )).toJSArray
                )
            )
            .setYAxis(
              List(
                YAxisOptions()
                  .setTitle(YAxisTitleOptions().setText("Elevation"))
                  .setAllowDecimals(false)
                  .setMin(0)
                  .setMax(90)
                  .setTickInterval(10)
                  .setMinorTickInterval(5)
                  .setLabels(YAxisLabelsOptions().setFormat("{value}°")),
                YAxisOptions()
                  .setOpposite(true)
                  .setTitle(YAxisTitleOptions().setText("Parallactic angle"))
                  .setMin(-180)
                  .setMax(180)
                  .setTickInterval(60)
                  .setClassName("plot-axis-parallactic-angle")
                  .setShowEmpty(false)
                  .setLabels(YAxisLabelsOptions().setFormat("{value}°")),
                YAxisOptions()
                  .setOpposite(true)
                  .setTitle(YAxisTitleOptions().setText("Sky Brightness [V] (mag/arcsec²)"))
                  .setMin(17)
                  .setMax(22)
                  .setReversed(true)
                  .setTickInterval(1)
                  .setClassName("plot-axis-sky-brightness")
                  .setShowEmpty(false)
                  .setLabels(YAxisLabelsOptions().setFormat("{value}"))
                  .setPlotLines:
                    if (shownSeries.get.contains(ElevationSeries.SkyBrightness))
                      SkyBrightnessPercentileLines.toJSArray
                    else
                      js.Array()
                  .setPlotBands:
                    if (shownSeries.get.contains(ElevationSeries.SkyBrightness))
                      SkyBrightnessPercentileBands.toJSArray
                    else
                      js.Array()
              ).toJSArray
            )
            .setPlotOptions(
              PlotOptions()
                .setSeries(
                  PlotSeriesOptions()
                    .setLineWidth(4)
                    .setMarker(PointMarkerOptionsObject().setEnabled(false).setRadius(0))
                    .setStates(
                      SeriesStatesOptionsObject()
                        .setHover(SeriesStatesHoverOptionsObject().setEnabled(false))
                    )
                )
            )
            .setSeries(
              ElevationSeries.values
                .map: series =>
                  val zones: Option[js.Array[SeriesZonesOptionsObject]] =
                    pendingTime.map: pt =>
                      js.Array(
                        SeriesZonesOptionsObject()
                          .setValue(start.toEpochMilli.toDouble),
                        SeriesZonesOptionsObject()
                          .setValue(start.plus(pt).toEpochMilli.toDouble)
                          .setClassName("elevation-plot-visualization-period")
                      )

                  val baseSeries: List[SeriesAreaOptions] =
                    seriesData.toList.map: (id, targetSeriesData) =>
                      val name: String = plotData
                        .value(id)
                        .map(_.name.value)
                        .getOrElse(id.value.fold(_.toString, _.toString))
                      SeriesAreaOptions((), (), ())
                        .setName(s"${series.name} ($name)")
                        .setClassName("elevation-plot-series")
                        .setYAxis(series.yAxis)
                        .setData(series.data(targetSeriesData.chartData).toJSArray)
                        .setVisible:
                          series.enabled(opts).isVisible && shownSeries.get.contains(series)
                        .setEvents:
                          SeriesEventsOptionsObject()
                            .setHide: (s, _) =>
                              props.hideSeriesCB(shownSeries, series, s.chart).runNow()
                            .setShow: (s, _) =>
                              props.showSeriesCB(shownSeries, series, s.chart).runNow()
                        .setFillOpacity(0)
                        .setZoneAxis("x")

                  val zonedSeries: List[SeriesAreaOptions] =
                    zones.fold(baseSeries)(z => baseSeries.map(_.setZones(z)))

                  series match // Adjust fill area to axis
                    case ElevationSeries.SkyBrightness    =>
                      zonedSeries.map(_.setThreshold(22))
                    case ElevationSeries.ParallacticAngle =>
                      zonedSeries.map(_.setThreshold(-180))
                    case _                                => zonedSeries
                .flatMap(_.map(_.asInstanceOf[SeriesOptionsType]))
                .toJSArray
            )

          (options, seriesData.headOption.map(_._2.moonData))
      .render: (props, shownSeries, computed) =>
        React.Fragment(
          Chart(computed.map(_._1)),
          computed._2.map: moonData =>
            MoonPhase(moonData.moonPhase)(<.small("%1.0f%%".format(moonData.moonIllum * 100)))
        )
