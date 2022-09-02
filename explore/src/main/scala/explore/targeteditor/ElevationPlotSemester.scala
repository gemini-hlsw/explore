// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all._
import crystal.react.hooks._
import crystal.react.implicits._
import crystal.react.reuse._
import explore.events.PlotMessage.*
import explore.highcharts.*
import explore.implicits._
import explore.model.WorkerClients.PlotClient
import explore.model.boopickle.CommonPicklers.given
import explore.syntax.*
import explore.syntax.given
import explore.syntax.ui.given
import fs2.Stream
import gpp.highcharts.highchartsStrings.line
import gpp.highcharts.mod.XAxisLabelsOptions
import gpp.highcharts.mod._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enums.Site
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.model.Semester
import lucuma.core.syntax.time._
import lucuma.ui.reusability._
import lucuma.ui.syntax.all.given
import react.common.GenericComponentPAF2VdomNode
import react.common.ReactFnProps
import react.highcharts.ResizingChart
import react.resizeDetector.hooks._
import spire.math.Bounded

import java.time.Duration
import java.time.Instant
import java.time.LocalTime
import java.time.ZoneOffset
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.collection.immutable.TreeMap
import scala.scalajs.js

import js.JSConverters._

case class ElevationPlotSemester(
  site:             Site,
  coords:           Coordinates,
  semester:         Semester
)(implicit val ctx: AppContextIO)
    extends ReactFnProps(ElevationPlotSemester.component)

object ElevationPlotSemester {
  private type Props = ElevationPlotSemester

  private val PlotDayRate: Long     = 3
  private val MillisPerHour: Double = 60 * 60 * 1000
  private val MillisPerDay: Double  = MillisPerHour * 24

  private val dateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useResizeDetector()
      .useSerialState(none[Chart_])
      .useStreamResourceBy((_, _, chartOpt) => chartOpt)((props, _, _) =>
        chartOpt =>
          chartOpt.value.value
            .map { chart =>
              given AppContextIO = props.ctx

              val series = chart.series(0)
              val xAxis  = chart.xAxis(0)

              PlotClient[IO]
                .request(
                  RequestSemesterSidereal(props.semester, props.site, props.coords, PlotDayRate)
                )
                .map(
                  _.evalMap { case SemesterPoint(instant, visibility) =>
                    val value = instant.toDouble
                    IO(xAxis.removePlotLine("progress")) >>
                      IO(
                        xAxis.addPlotLine(
                          AxisPlotLinesOptions
                            .XAxisPlotLinesOptions()
                            .setId("progress")
                            .setValue(value)
                            .setZIndex(1000)
                            .setClassName("plot-plot-line-progress")
                        )
                      ) >>
                      IO {
                        series.addPoint(
                          PointOptionsObject(accessibility = js.undefined)
                            .setX(value)
                            // Trick to leave small values out of the plot
                            .setY(if (visibility > 0.2) visibility / MillisPerHour else -1),
                          redraw = true,
                          shift = false,
                          animation = false
                        )
                      }
                  } ++ fs2.Stream(xAxis.removePlotLine("progress"))
                )
            }
            .getOrElse(Resource.pure(fs2.Stream()))
      )
      .useEffectWithDepsBy((_, resize, _, _) => resize)((_, _, chartOpt, _) =>
        resize =>
          (resize.height, resize.width, chartOpt.value.value)
            .mapN((height, width, chart) => Callback(chart.setSize(width, height)))
            .orEmpty
      )
      .render { (props, resize, chartOpt, _) =>
        implicit val ct = props.ctx

        def timeFormat(value: Double): String =
          ZonedDateTime
            .ofInstant(Instant.ofEpochMilli(value.toLong), props.site.timezone)
            .format(dateTimeFormatter)

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
            .format(dateTimeFormatter)

        val tooltipFormatter: TooltipFormatterCallbackFunction = {
          (ctx: TooltipFormatterContextObject, _: Tooltip) =>
            val x          = ctx.x match
              case x: Double => x
              case x: String => x.toDouble
              case _         => 0.0
            val y          = ctx.y match
              case y: Double => y
              case y: String => y.toDouble
              case _         => 0.0
            val date       = dateFormat(x)
            val visibility = Duration.ofMillis((y * MillisPerHour).toLong)
            val minutes    = visibility.getSeconds / 60
            s"<strong>$date</strong><br/>${ctx.series.name}: ${minutes / 60}h${minutes % 60}m"
        }

        val options = Options()
          .setChart(
            commonOptions
              .setHeight(resize.height.getOrElse(1).toDouble)
          )
          .setTitle(
            TitleOptions().setText(
              s"Semester ${props.semester.format}"
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
              .setMin(props.semester.start.atSite(props.site).toInstant.toEpochMilli.toDouble)
              .setMax(props.semester.end.atSite(props.site).toInstant.toEpochMilli.toDouble)
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
                  .setMarker(PointMarkerOptionsObject().setEnabled(false))
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
            )
              .map(_.asInstanceOf[SeriesOptionsType])
              .toJSArray
          )

        <.div(
          ResizingChart(options, c => chartOpt.setState(c.some))
            .withKey(s"$props")
            .when(resize.height.isDefined)
        ).withRef(resize.ref)
      }
}
