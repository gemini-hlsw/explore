// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.plots

import cats.Semigroupal
import cats.syntax.all.*
import crystal.react.*
import explore.*
import explore.highcharts.*
import explore.model.Constants
import explore.model.enums.TimeDisplay
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Site
import lucuma.core.enums.TwilightType
import lucuma.core.math.Angle
import lucuma.core.math.BoundedInterval
import lucuma.core.math.skycalc.ImprovedSkyCalc
import lucuma.core.model.ObservingNight
import lucuma.core.model.TwilightBoundedNight
import lucuma.core.util.Enumerated
import lucuma.core.util.time.*
import lucuma.react.common.ReactFnProps
import lucuma.react.highcharts.Chart
import lucuma.typed.highcharts.mod.*
import lucuma.ui.components.MoonPhase
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import org.typelevel.cats.time.given

import java.time.Duration
import java.time.Instant
import java.time.ZoneId
import java.time.ZoneOffset
import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import scala.collection.MapView
import scala.scalajs.js

import js.JSConverters.*

case class NightPlot(
  plotData:         PlotData,
  coordsTime:       Instant,
  excludeIntervals: List[BoundedInterval[Instant]],
  obsTime:          Option[Instant],
  obsDuration:      Option[Duration],
  options:          View[ObjectPlotOptions],
  emptyMessage:     String,
  hideTargetLabel:  Boolean
) extends ReactFnProps(NightPlot.component)

object NightPlot:
  private type Props = NightPlot

  private val MillisPerHour: Double = 60 * 60 * 1000

  private def formatAngle(degs: Double): String =
    val dms     = Angle.DMS(Angle.fromDoubleDegrees(degs))
    val degrees = if (dms.degrees > 180) s"-${360 - dms.degrees}" else dms.degrees.toString
    val minutes = "%02d".format(dms.arcminutes)
    val seconds = "%02d".format(dms.arcseconds)
    s"$degrees°$minutes′$seconds″"

  private val ElevationMinimumLine =
    YAxisPlotLinesOptions()
      .setValue(30)
      .setClassName("plot-elevation-minimum")
      .setZIndex(1)

  private val SkyBrightnessPercentileLines: List[YAxisPlotLinesOptions] =
    def plotLine(id: String, value: Double): YAxisPlotLinesOptions =
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
    def plotBand(id: String, label: String, from: Double, to: Double): YAxisPlotBandsOptions =
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

  private case class ChartSeriesData(
    seriesType:             SeriesType,
    objectPlotData:         ObjectPlotData,
    objectSeriesData:       ObjectPlotData.SeriesData,
    visiblePlots:           List[SeriesType],
    shouldHideTargetLabels: Boolean
  ):
    lazy val name: String               =
      if (seriesType === SeriesType.LunarElevation) "Moon" else objectPlotData.name.value
    lazy val yAxis: Int                 = seriesType.yAxis
    lazy val visible: Boolean           = visiblePlots.contains_(seriesType)
    // Moon elevation plot is always solid
    lazy val sites: List[Site]          =
      if (seriesType === SeriesType.LunarElevation) Enumerated[Site].all else objectPlotData.sites
    lazy val data: js.Array[Chart.Data] = seriesType.data(objectSeriesData)
    // SkyBrightness can be out of bounds, in that case we hide the label (otherwise it's confusingly shown at the top of the chart).
    // Hide elevation and parallactic angle labels when there's only one target or all targets have the same observation ID.
    // Sky brightness band labels remain visible as they provide valuable sky condition context.
    lazy val showLabel: Boolean         = seriesType match
      case SeriesType.Elevation        =>
        !shouldHideTargetLabels
      case SeriesType.ParallacticAngle =>
        !shouldHideTargetLabels
      case SeriesType.SkyBrightness    =>
        !shouldHideTargetLabels && data.exists: point =>
          point
            .asInstanceOf[PointOptionsObject]
            .y // Deal with js.UndefOr[Double | Null] type
            .toOption
            .flatMap(v => Option(v.asInstanceOf[Double]))
            .exists(_ > 17)
      case _                           => true

  private def createChartOptions(
    plotData:         PlotData,
    opts:             ObjectPlotOptions,
    chartAndMoonData: (MapView[ObjectPlotData.Id, ObjectPlotData.SeriesData],
                       Option[ObjectPlotData.MoonData]
    ),
    obsTime:          Option[Instant],
    obsDuration:      Option[Duration],
    excludeIntervals: List[BoundedInterval[Instant]],
    hideLabel:        Boolean,
    observingNight:   ObservingNight
  ): Options =
    val isSingleTargetPlot: Boolean = plotData.value.size === 1

    val shouldHideTargetLabels: Boolean = isSingleTargetPlot || hideLabel

    val chartData: MapView[ObjectPlotData.Id, ObjectPlotData.SeriesData] =
      chartAndMoonData._1

    val site: Site               = opts.site
    val timeDisplay: TimeDisplay = opts.timeDisplay

    val tbNauticalNight: TwilightBoundedNight =
      observingNight.twilightBoundedUnsafe(TwilightType.Nautical)

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
      (
        labelValue: AxisLabelsFormatterContextObject,
        _:          AxisLabelsFormatterContextObject
      ) => timeFormat(labelValue.value.asInstanceOf[Double])

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
        val seriesIndex: Int = ctx.series.index.toInt / chartData.size.toInt
        val value            = seriesIndex match
          case 0 =>                      // Target elevation with airmass
            formatAngle(y) + s"<br/>Airmass: ${"%.3f".format(ctx.point.asInstanceOf[ElevationPointWithAirmass].airmass)}"
          case 2 => "%.2f".format(ctx.y) // Sky Brightness
          case _ => formatAngle(y)       // Other elevations

        s"<strong>$time ($timeDisplayStr)</strong><br/>${ctx.series.name}: $value"

    val dusk: String = instantFormat(tbNauticalNight.start)
    val dawn: String = instantFormat(tbNauticalNight.end)

    val seriesToPlot: List[ChartSeriesData] =
      Semigroupal[List]
        .product(
          chartData.toList
            .map: (id, targetChartData) =>
              plotData.value
                .get(id)
                .map(targetPlotData => (targetPlotData, targetChartData))
            .zipWithIndex,
          SeriesType.values.toList
        )
        .collect: // Plot lunar elevation only once.
          case ((Some((targetPlotData, targetChartData)), index), seriesType)
              if seriesType =!= SeriesType.LunarElevation || index === 0 =>
            ChartSeriesData(
              seriesType,
              targetPlotData,
              targetChartData,
              opts.visiblePlots,
              shouldHideTargetLabels
            )

    val targetsBelowHorizonStr: Option[String] =
      Option.when(
        seriesToPlot.forall: series =>
          series.seriesType match
            case SeriesType.Elevation =>
              series.data.forall: point =>
                point
                  .asInstanceOf[PointOptionsObject]
                  .y // Deal with js.UndefOr[Double | Null] type
                  .toOption
                  .flatMap(v => Option(v.asInstanceOf[Double]))
                  .forall(_ <= 0)
            case _                    => true
      ):
        if isSingleTargetPlot then "Target is below horizon"
        else "All targets are below horizon"

    // Fill out observation time
    val zones: Option[js.Array[SeriesZonesOptionsObject]] =
      (obsTime, obsDuration).mapN: (t, d) =>
        js.Array(
          SeriesZonesOptionsObject()
            .setValue(t.toEpochMilli.toDouble),
          SeriesZonesOptionsObject()
            .setValue(t.plus(d).toEpochMilli.toDouble)
            .setClassName("elevation-plot-visualization-period")
        )

    Options()
      .setChart(commonOptions)
      .setLegend(LegendOptions().setEnabled(false))
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
                  .setFrom(opts.minInstant.toEpochMilli.toDouble)
                  .setTo(tbNauticalNight.start.toEpochMilli.toDouble)
                  .setClassName("plot-band-twilight-nautical")
                  // We need z-index > 0 to display over grid. But not too high, or it will display over tooltips.
                  .setZIndex(1)
                  .setLabel(
                    XAxisPlotBandsLabelOptions()
                      .setText(s"  Evening 12° - Twilight: $dusk")
                      .setRotation(270)
                      .setAlign(AlignValue.right)
                      .setTextAlign(AlignValue.center)
                      .setVerticalAlign(VerticalAlignValue.middle)
                  ),
                XAxisPlotBandsOptions() // Empty bands don't work on highcharts 11.4.8. Instead we create the same band in revese and no fill
                  .setFrom(tbNauticalNight.end.toEpochMilli.toDouble)
                  .setTo(opts.maxInstant.toEpochMilli.toDouble)
                  .setClassName("plot-band-twilight-nautical-end")
                  .setZIndex(1)
                  .setLabel:
                    XAxisPlotBandsLabelOptions()
                      .setText(s"  Morning 12° - Twilight: $dawn")
                      .setRotation(270)
                      .setAlign(AlignValue.left)
                      .setTextAlign(AlignValue.center)
                      .setVerticalAlign(VerticalAlignValue.middle)
              )).toJSArray
          )
      )
      .setYAxis:
        (
          List(
            YAxisOptions()
              .setTitle(YAxisTitleOptions().setText("Elevation"))
              .setAllowDecimals(false)
              .setMin(0)
              .setMax(90)
              .setTickInterval(10)
              .setMinorTickInterval(5)
              .setShowEmpty(false)
              .setLabels(YAxisLabelsOptions().setFormat("{value}°"))
              .setPlotLines(js.Array(ElevationMinimumLine)),
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
                if (opts.visiblePlots.contains_(SeriesType.SkyBrightness))
                  SkyBrightnessPercentileLines.toJSArray
                else
                  js.Array()
              .setPlotBands:
                if (opts.visiblePlots.contains_(SeriesType.SkyBrightness))
                  SkyBrightnessPercentileBands.toJSArray
                else
                  js.Array()
          ) ++
            (if (opts.visiblePlots.contains_(SeriesType.Elevation))
               List(
                 YAxisOptions()
                   .setOpposite(true)
                   .setLinkedTo(0)
                   .setTitle(YAxisTitleOptions().setText("Airmass"))
                   .setAllowDecimals(true)
                   .setTickInterval(10)
                   .setMinorTickInterval(5)
                   .setLabels:
                     YAxisLabelsOptions().setFormatter:
                       (
                         labelValue: AxisLabelsFormatterContextObject,
                         _:          AxisLabelsFormatterContextObject
                       ) =>
                         val h: Double = labelValue.value.asInstanceOf[Double]
                         if h > 0 then
                           "%.2f".format: // Pickering (2002) method
                             1 / (math
                               .sin(
                                 math.toRadians(h + 244 / (165 + 47 * math.pow(h, 1.1)))
                               ))
                         else ""
               )
             else List.empty)
        ).toJSArray
      .setPlotOptions:
        PlotOptions()
          .setSeries(
            PlotSeriesOptions()
              .setLineWidth(4)
              .setMarker(PointMarkerOptionsObject().setEnabled(false).setRadius(0))
              .setStates:
                SeriesStatesOptionsObject()
                  .setHover(SeriesStatesHoverOptionsObject().setEnabled(false))
          )
      .setSeries:
        seriesToPlot
          .map: series =>
            val baseSeries: SeriesAreaOptions =
              SeriesAreaOptions((), (), ())
                .setName(if (series.showLabel) series.name else "")
                .setLabel:
                  SeriesLabelOptionsObject()
                    .setEnabled(series.showLabel)
                    .setConnectorAllowed(true)
                    .setOnArea(false)
                .setClassName:
                  "elevation-plot-series" +
                    (if (!series.sites.contains_(site)) " highcharts-dashed-series"
                     else "")
                .setYAxis(series.yAxis)
                .setData(series.data)
                .setVisible(series.visible)
                .setFillOpacity(0)
                .setZoneAxis("x")
                .setThreshold: // Ensure that the zones are always painted towards the bottom
                  if (series.seriesType === SeriesType.SkyBrightness)
                    Double.PositiveInfinity
                  else Double.NegativeInfinity

            zones
              .fold(baseSeries)(z => baseSeries.setZones(z))
              .asInstanceOf[SeriesOptionsType]
          .toJSArray

  private val component = ScalaFnComponent[Props]: props =>
    val observingNight =
      ObservingNight.fromSiteAndLocalDate(props.options.get.site, props.options.get.date)

    val bounds =
      val start = props.options.get.minInstant
      val end   = props.options.get.maxInstant

      (start, end)

    for {
      chartAndMoonData <- useMemo((props.options.get.site, props.plotData, bounds)):
                            (site, plotData, bounds) =>
                              val (start, end): (Instant, Instant) = bounds

                              val seriesData: MapView[ObjectPlotData.Id, ObjectPlotData.Points] =
                                plotData.value.view.mapValues(_.pointsAtInstant(site, start, end))

                              val chartData: MapView[ObjectPlotData.Id, ObjectPlotData.SeriesData] =
                                seriesData.mapValues(_.seriesData)

                              (chartData, seriesData.headOption.map(_._2.moonData))
      chartOptions     <-
        useMemo(
          (props.plotData,
           props.options.get,
           chartAndMoonData,
           props.obsTime,
           props.obsDuration,
           props.excludeIntervals,
           props.hideTargetLabel
          )
        ): (plotData, opts, chartAndMoonData, obsTime, obsDuration, excludeIntervals, hideLabel) =>
          createChartOptions(
            plotData,
            opts,
            chartAndMoonData,
            obsTime,
            obsDuration,
            excludeIntervals,
            hideLabel,
            observingNight
          )
      chartOpt         <- useRef(none[Chart_]) // chart handler (chartOpt)
      _                <- useEffectWithDeps((props.plotData.value.size, chartOpt.value.void)): (size, _) =>
                            Callback:
                              if size === 0 then chartOpt.value.foreach(_.showLoading(props.emptyMessage))
                              else chartOpt.value.foreach(_.hideLoading())
    } yield React.Fragment(
      Chart(chartOptions, allowUpdate = false, onCreate = c => chartOpt.set(c.some)),
      chartAndMoonData._2.map: moonData =>
        MoonPhase(moonData.moonPhase)(<.small("%1.0f%%".format(moonData.moonIllum * 100)))
    )
