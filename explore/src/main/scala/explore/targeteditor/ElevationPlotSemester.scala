// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import crystal.react.hooks.*
import explore.events.PlotMessage.*
import explore.highcharts.*
import explore.model.AppContext
import explore.model.Constants
import explore.model.WorkerClients.PlotClient
import fs2.Stream
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Coordinates
import lucuma.core.model.CoordinatesAtVizTime
import lucuma.react.common.ReactFnProps
import lucuma.react.highcharts.ResizingChart
import lucuma.react.resizeDetector.hooks.*
import lucuma.typed.highcharts.highchartsStrings.line
import lucuma.typed.highcharts.mod.*
import lucuma.ui.syntax.all.given

import java.time.Duration
import java.time.Instant
import java.time.LocalDateTime
import java.time.LocalTime
import java.time.ZoneOffset
import java.time.ZonedDateTime
import scala.concurrent.duration.*
import scala.scalajs.js

import js.JSConverters.*
import explore.model.ElevationPlotOptions

case class ElevationPlotSemester(
  options:          ElevationPlotOptions,
  coords:           CoordinatesAtVizTime,
  excludeIntervals: List[BoundedInterval[Instant]]
) extends ReactFnProps(ElevationPlotSemester.component)

object ElevationPlotSemester:
  private type Props = ElevationPlotSemester

  private val PlotDayRate: Long     = 3
  private val MillisPerHour: Double = 60 * 60 * 1000
  private val MillisPerDay: Double  = MillisPerHour * 24
  private val MinVisibility: Double = 0.2 // 12 minutes

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useResizeDetector()
      .useSerialState(none[Chart_])
      .useEffectWithDepsBy((props, _, _, chartOpt) => chartOpt.value.isDefined)(
        (props, ctx, _, chartOpt) =>
          _ =>
            chartOpt.value.value
              .map { chart =>
                import ctx.given

                val series = chart.series(0)
                val xAxis  = chart.xAxis(0)

                PlotClient[IO]
                  .request(
                    RequestSemesterSidereal(
                      props.options.semester,
                      props.options.site,
                      props.coords.value,
                      PlotDayRate
                    )
                  )
                  .map(
                    _.groupWithin(100, 1500.millis)
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
                      fs2.Stream(xAxis.removePlotLine("progress"))
                  )
              }
              .getOrElse(Resource.pure(fs2.Stream()))
              .use(_.compile.drain)
      )
      .useEffectWithDepsBy((props, _, _, chartOpt) => (props.options.date, chartOpt))(
        (props, _, _, _) =>
          (date, chartOpt) =>
            CallbackTo(chartOpt.value.value) >>=
              (_.map(chart =>
                Callback {
                  // 2pm of the selected day, same as for semester start and end
                  val localDateTime: LocalDateTime =
                    LocalDateTime.of(date, LocalTime.MIDNIGHT).plusHours(14)
                  val zonedDateTime: ZonedDateTime =
                    ZonedDateTime.of(localDateTime, props.options.site.timezone)

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
              ).orEmpty)
      )
      .render { (props, _, resize, chartOpt) =>
        def timeFormat(value: Double): String =
          ZonedDateTime
            .ofInstant(Instant.ofEpochMilli(value.toLong), props.options.site.timezone)
            .format(Constants.GppDateFormatter)

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
            .format(Constants.GppDateFormatter)

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

        val semester = props.options.semester
        val site     = props.options.site

        val options = Options()
          .setChart(
            commonOptions
              .setHeight(resize.height.getOrElse(1).toDouble)
              .setAnimation(false)
          )
          .setTitle(
            TitleOptions().setText(
              s"Semester ${semester.format}"
            )
          )
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
                props.excludeIntervals
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
                .setTitle(YAxisTitleOptions().setText("Hours"))
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
              SeriesLineOptions((), (), line)
                .setName("Visibility")
                .setYAxis(0)
                .setAnimation(false)
            )
              .map(_.asInstanceOf[SeriesOptionsType])
              .toJSArray
          )

        <.div(
          ResizingChart(options, c => chartOpt.setState(c.some))
            .withKey(
              s"$site-${props.coords}-$semester-${resize}-${props.excludeIntervals}"
            )
            .when(resize.height.isDefined)
        ).withRef(resize.ref)
      }
