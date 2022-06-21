// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Eval
import cats.effect.IO
import cats.syntax.all._
import crystal.react.hooks._
import crystal.react.implicits._
import explore.implicits._
import fs2.Stream
import gpp.highcharts.highchartsStrings.line
import gpp.highcharts.mod.XAxisLabelsOptions
import gpp.highcharts.mod._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enums.Site
import lucuma.core.enums.TwilightType
import lucuma.core.math.Coordinates
import lucuma.core.math.Declination
import lucuma.core.math.skycalc.solver.ElevationSolver
import lucuma.core.math.skycalc.solver.Samples
import lucuma.core.model.Semester
import lucuma.core.model.TwilightBoundedNight
import lucuma.core.syntax.boundedInterval._
import lucuma.core.syntax.time._
import lucuma.ui.reusability._
import org.typelevel.cats.time._
import react.common._
import react.highcharts.Chart
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

final case class ElevationPlotSemester(
  site:             Site,
  coords:           Coordinates,
  semester:         Semester
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[ElevationPlotSemester](ElevationPlotSemester.component)

final case class SemesterPlotCalc(semester: Semester, site: Site) {
  protected val SampleRate: Duration = Duration.ofMinutes(1)

  protected val MinTargetElevation = Declination.fromDoubleDegrees(30.0).get

  val interval: Bounded[Instant] =
    Bounded.unsafeOpenUpper(semester.start.atSite(site).toInstant,
                            semester.end.atSite(site).toInstant
    )

  def samples(coordsForInstant: Instant => Coordinates, dayRate: Long): Samples[Duration] = {
    val results = Samples
      .atFixedRate(
        Bounded.unsafeOpenUpper(
          semester.start.atSite(site).toInstant,
          semester.end.atSite(site).toInstant
        ),
        SampleRate
      )(coordsForInstant)
      .toSkyCalResultsAt(site.place)

    val targetVisible = ElevationSolver(MinTargetElevation, Declination.Max).solve(results) _

    Samples.fromMap(
      Iterator
        .iterate(semester.start.localDate)(_.plusDays(dayRate))
        .takeWhile(_ <= semester.end.localDate)
        .map { date =>
          val instant = date.atTime(LocalTime.MIDNIGHT).atZone(site.timezone).toInstant
          instant -> Eval.later {
            // We could increment a progress bar here.
            // Better yet, let's do a "point producer" that runs an effect for each produced point, which could be to add it to the chart.
            val night     = TwilightBoundedNight
              .fromTwilightTypeAndSiteAndLocalDateUnsafe(TwilightType.Nautical, site, date)
            val intervals = targetVisible(night.interval)
            intervals.duration
          }
        }
        .to(TreeMap)
    )
  }
}

object ElevationPlotSemester {
  type Props = ElevationPlotSemester

  private val PlotDayRate: Long     = 3
  private val MillisPerHour: Double = 60 * 60 * 1000
  private val MillisPerDay: Double  = MillisPerHour * 24

  val dateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")

  implicit val propsReuse: Reusability[Props]           = Reusability.derive
  implicit val taksReuse: Reusability[Option[IO[Unit]]] = Reusability.always

  val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateViewWithReuse(none[IO[Unit]])
      .useResizeDetector()
      // This component can't be updated, it must be rerendered. Don't add reuse
      .renderWithReuse { (props, cancel, resize) =>
        implicit val ct = props.ctx

        val plotter = SemesterPlotCalc(props.semester, props.site)

        def renderPoints(chart: Chart_): IO[IO[Unit]] = {
          val series = chart.series(0)
          val xAxis  = chart.xAxis(0)
          Stream
            .fromIterator[IO](plotter.samples(_ => props.coords, PlotDayRate).iterator, 1)
            .evalMap { case (instant, visibilityDuration) =>
              val value = instant.toEpochMilli.toDouble
              IO(
                xAxis.addPlotLine(
                  AxisPlotLinesOptions
                    .XAxisPlotLinesOptions()
                    .setId("progress")
                    .setValue(value - PlotDayRate * MillisPerDay) // Draw line on last drawn point.
                    .setZIndex(1000)
                    .setClassName("plot-plot-line-progress")
                )
              ) >>
                IO.cede >>
                IO {
                  val visibility = visibilityDuration.toMillis
                  series.addPoint(
                    PointOptionsObject(accessibility = js.undefined)
                      .setX(value)
                      // Trick to leave small values out of the plot
                      .setY(if (visibility > 0.1) visibility / MillisPerHour else -1),
                    redraw = true,
                    shift = false,
                    animation = false
                  )
                } >>
                IO(xAxis.removePlotLine("progress"))
            }
            .compile
            .drain
            .start
            .map(_.cancel)
        }

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
            val date       = dateFormat(ctx.x)
            val visibility = Duration.ofMillis((ctx.y * MillisPerHour).toLong)
            val minutes    = visibility.getSeconds / 60
            s"<strong>$date</strong><br/>${ctx.series.name}: ${minutes / 60}h${minutes % 60}m"
        }

        val options = Options()
          .setChart(
            ChartOptions()
              .setHeight(resize.height.getOrElse(1).toDouble)
              .setStyledMode(true)
              .setAlignTicks(false)
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
              .setMin(plotter.interval.lower.toEpochMilli.toDouble)
              .setMax(plotter.interval.upper.toEpochMilli.toDouble)
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
                // .setData(series.toJSArray)
            )
              .map(_.asInstanceOf[SeriesOptionsType])
              .toJSArray
          )

        <.div(
          Chart(
            options,
            chart =>
              (renderPoints(chart) >>= (cancelToken =>
                cancel.set(cancelToken.some).to[IO]
              )).runAsync
          )
            .withKey(s"$props-$resize")
            .when(resize.height.isDefined)
        )
          .withRef(resize.ref)
      }
}
