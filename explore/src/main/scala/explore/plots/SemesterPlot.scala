// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.plots

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import explore.events.PlotMessage.*
import explore.highcharts.*
import explore.model.AppContext
import explore.model.WorkerClients.PlotClient
import fs2.Stream
import japgolly.scalajs.react.*
import lucuma.core.math.BoundedInterval
import lucuma.core.model.CoordinatesAt
import lucuma.core.util.time.format.GppDateFormatter
import lucuma.react.common.ReactFnProps
import lucuma.react.highcharts.Chart
import lucuma.typed.highcharts.mod.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import org.typelevel.cats.time.given

import java.time.Duration
import java.time.Instant
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.ZoneOffset
import java.time.ZonedDateTime
import scala.concurrent.duration.*
import scala.scalajs.js

import js.JSConverters.*

case class SemesterPlot(
  options:          ObjectPlotOptions,
  coords:           CoordinatesAt,
  excludeIntervals: List[BoundedInterval[Instant]]
) extends ReactFnProps(SemesterPlot.component)

object SemesterPlot:
  private type Props = SemesterPlot

  private val PlotDayRate: Long     = 3
  private val MillisPerHour: Double = 60 * 60 * 1000
  private val MillisPerDay: Double  = MillisPerHour * 24
  private val MinVisibility: Double = 0.2 // 12 minutes

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useState(none[Chart_]) // chart handler (chartOpt)
      .useEffectStreamResourceWithDepsBy((props, _, chartOpt) =>
        chartOpt.value.map: chart =>
          (props.options.semester, props.options.site, props.coords.value, Reusable.always(chart))
      ): (_, ctx, _) =>
        _.map { (semester, site, coords, chart) =>
          import ctx.given

          val series = chart.series(0)
          val xAxis  = chart.xAxis(0)

          PlotClient[IO]
            .request:
              RequestSemesterSidereal(semester, site, coords, PlotDayRate)
            .map(updateStream =>
              // Empty the series data
              fs2.Stream.eval(IO(series.setData(js.Array(), redraw = true))) >>
                fs2.Stream(chart.showLoading("Computing...")) ++
                fs2.Stream(xAxis.removePlotLine("progress")) ++ // Clear previous progress line
                updateStream
                  .groupWithin(100, 1500.millis)
                  .evalMap { chunk =>
                    IO(xAxis.removePlotLine("progress")) >>
                      IO {
                        chunk.toList
                          .map { case SemesterPoint(instant, visibility) =>
                            val instantD: Double    = instant.toDouble
                            val visibilityD: Double = visibility / MillisPerHour

                            series.addPoint(
                              PointOptionsObject().setAccessibilityUndefined
                                .setX(instantD)
                                // Trick to leave small values out of the plot
                                .setY(if (visibilityD > MinVisibility) visibilityD else -1),
                              redraw = true,
                              shift = false,
                              animation = false
                            )

                            (instantD, visibilityD)
                          }
                          .foldLeft((0.0, 0.0)) {
                            case ((maxInstantD, maxVisibilityD), (instantD, visibilityD)) =>
                              (instantD.max(maxInstantD), visibilityD.max(maxVisibilityD))
                          }
                      }
                        .flatTap { case (maxInstant, _) =>
                          IO(
                            xAxis.addPlotLine(
                              AxisPlotLinesOptions
                                .XAxisPlotLinesOptions()
                                .setId("progress")
                                .setValue(maxInstant)
                                .setZIndex(1000)
                                .setClassName("plot-plot-line-progress")
                            )
                          ).void
                        }
                        .map { case (_, maxVisibility) => maxVisibility }
                  }
                  .scan1(math.max)
                  .last
                  .evalMap(
                    _.filterNot(_ > MinVisibility)
                      .map(_ =>
                        IO(
                          xAxis.setTitle(XAxisTitleOptions().setText("Target is below horizon"))
                        ).void
                      )
                      .orEmpty
                  ) ++
                fs2.Stream(xAxis.removePlotLine("progress")) ++
                fs2.Stream(chart.hideLoading())
            )
        }.orEmpty
      .useEffectWithDepsBy((props, _, chartOpt) =>
        (props.options.date, props.options.site, chartOpt.value.isDefined)
      ): (_, _, chartOpt) =>
        (date, site, _) =>
          chartOpt.value
            .map(chart =>
              Callback {
                // 2pm of the selected day, same as for semester start and end
                val localDateTime: LocalDateTime =
                  LocalDateTime.of(date, LocalTime.MIDNIGHT).plusHours(14)
                val zonedDateTime: ZonedDateTime =
                  ZonedDateTime.of(localDateTime, site.timezone)

                // Axes maybe undefined or empty when remounting.
                if (!js.isUndefined(chart.axes) && chart.axes.length > 0) {
                  val xAxis = chart.xAxis(0)
                  xAxis.removePlotLine("date")
                  xAxis.addPlotLine(
                    AxisPlotLinesOptions
                      .XAxisPlotLinesOptions()
                      .setId("date")
                      .setValue(zonedDateTime.toInstant.toEpochMilli.toDouble)
                      .setZIndex(1000)
                      .setClassName("plot-plot-line-date")
                  )
                  ()
                }
              }
            )
            .orEmpty
      .useMemoBy((props, _, _) =>
        (props.options.semester, props.options.site, props.excludeIntervals)
      ): (_, _, _) =>
        (semester, site, excludeIntervals) =>
          def timeFormat(value: Double): String =
            ZonedDateTime
              .ofInstant(Instant.ofEpochMilli(value.toLong), site.timezone)
              .format(GppDateFormatter)

          val tickFormatter: AxisLabelsFormatterCallbackFunction =
            (
              labelValue: AxisLabelsFormatterContextObject, // [Double],
              _:          AxisLabelsFormatterContextObject  // [String]
            ) =>
              (labelValue.value: Any) match {
                case ms: Double => timeFormat(ms)
                case s          => s.toString
              }

          def dateFormat(value: Double): String =
            ZonedDateTime
              .ofInstant(Instant.ofEpochMilli(value.toLong), ZoneOffset.UTC)
              .format(GppDateFormatter)

          val tooltipFormatter: TooltipFormatterCallbackFunction = {
            (ctx: TooltipFormatterContextObject, _: Tooltip) =>
              val x          = ctx.x match
                case x: Double => x
                case x: String => x.toDouble
                case _         => 0.0
              val y          = ctx.y.asInstanceOf[js.UndefOr[String | Double]] match
                case y: Double => y
                case y: String => y.toDouble
                case _         => 0.0
              val date       = dateFormat(x)
              val visibility = Duration.ofMillis((y * MillisPerHour).toLong)
              val minutes    = visibility.getSeconds / 60
              s"<strong>$date</strong><br/>${ctx.series.name}: ${minutes / 60}h${minutes % 60}m"
          }

          Options()
            .setChart(commonOptions.setAnimation(false))
            .setLegend(LegendOptions().setEnabled(false))
            .setTitle(TitleOptions().setText(s"Semester ${semester.format}"))
            .setCredits(CreditsOptions().setEnabled(false))
            .setTooltip(TooltipOptions().setFormatter(tooltipFormatter))
            .setXAxis(
              XAxisOptions()
                .setType(AxisTypeValue.datetime)
                .setLabels(XAxisLabelsOptions().setFormatter(tickFormatter))
                .setTickInterval(MillisPerDay * 10)
                .setMinorTickInterval(MillisPerDay * 5)
                .setMin(semester.start.atSite(site).toInstant.toEpochMilli.toDouble)
                .setMax(semester.end.atSite(site).toInstant.toEpochMilli.toDouble)
                .setPlotBands(
                  excludeIntervals
                    .map(window =>
                      XAxisPlotBandsOptions()
                        .setFrom(window.lower.toEpochMilli.toDouble)
                        .setTo(window.upper.toEpochMilli.toDouble)
                        .setClassName("plot-band-exclude-window")
                    )
                    .toJSArray
                )
            )
            .setYAxis(
              List(
                YAxisOptions()
                  .setTitle(YAxisTitleOptions().setText("Hours / Night"))
                  .setAllowDecimals(false)
                  .setMin(0)
                  .setMax(15)
                  .setTickInterval(1)
                  .setMinorTickInterval(0.5)
                  .setLabels(YAxisLabelsOptions().setFormat("{value}"))
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
              List(
                SeriesLineOptions((), ())
                  .setName("Visibility")
                  .setYAxis(0)
                  .setAnimation(false)
                  .setData(js.Array())
              )
                .map(_.asInstanceOf[SeriesOptionsType])
                .toJSArray
            )
      .render: (_, _, chartOpt, options) =>
        Chart(options, onCreate = c => chartOpt.setState(c.some))
