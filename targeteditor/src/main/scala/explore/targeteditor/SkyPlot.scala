// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all._
import explore.components.ui.ExploreStyles
import explore.implicits._
import gpp.highcharts.highchartsStrings.line
import gpp.highcharts.mod.XAxisLabelsOptions
import gpp.highcharts.mod._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.enum.Site
import lucuma.core.enum.TwilightType
import lucuma.core.math.Angle
import lucuma.core.math.Coordinates
import lucuma.core.model.ObservingNight
import lucuma.core.util.Enumerated
import lucuma.ui.reusability._
import monocle.macros.Lenses
import react.common._
import react.highcharts.Chart
import reactmoon.MoonPhase
import shapeless._

import java.time.Duration
import java.time.Instant
import java.time.LocalDate
import java.time.ZoneId
import java.time.ZoneOffset
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import scala.collection.immutable.HashSet
import scala.scalajs.js

import js.JSConverters._

final case class SkyPlot(
  site:   Site,
  coords: Coordinates,
  date:   LocalDate,
  zoneId: ZoneId,
  height: Int
) extends ReactProps[SkyPlot](SkyPlot.component)

object SkyPlot {
  type Props = SkyPlot

  @Lenses
  case class State(shownSeries: HashSet[ElevationSeries] = HashSet(ElevationSeries.Elevation))

  implicit private val propsReuse: Reusability[Props] = Reusability.derive
  // State doesn't trigger rerenders. We keep track of what is shown in case there is a
  // rerender due to a change of properties.
  implicit private val stateReuse: Reusability[State] = Reusability.always

  private val PlotEvery: Duration   = Duration.ofMinutes(1)
  private val MillisPerHour: Double = 60 * 60 * 1000

  protected case class SeriesData(
    targetAltitude:   List[Chart.Data],
    skyBrightness:    List[Chart.Data],
    parallacticAngle: List[Chart.Data],
    moonAltitude:     List[Chart.Data]
  )

  sealed abstract class ElevationSeries(
    val name:  String,
    val yAxis: Int,
    val data:  SeriesData => List[Chart.Data]
  )
  object ElevationSeries extends Enumerated[ElevationSeries] {
    case object Elevation        extends ElevationSeries("Elevation", 0, _.targetAltitude)
    case object ParallacticAngle extends ElevationSeries("Parallactic Angle", 1, _.parallacticAngle)
    case object SkyBrightness    extends ElevationSeries("Sky Brightness", 2, _.skyBrightness)
    case object LunarElevation   extends ElevationSeries("Lunar Elevation", 0, _.moonAltitude)

    def tag(a: ElevationSeries) = a.name

    val all = List(Elevation, ParallacticAngle, SkyBrightness, LunarElevation)
  }

  private val seriesDataGen = Generic[SeriesData]

  val dateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ofPattern("HH:mm")

  def formatAngle(degs: Double): String = {
    val dms     = Angle.DMS(Angle.fromDoubleDegrees(degs))
    val degrees = if (dms.degrees > 180) s"-${360 - dms.degrees}" else dms.degrees.toString
    val minutes = "%02d".format(dms.arcminutes)
    val seconds = "%02d".format(dms.arcseconds)
    s"$degrees°$minutes′$seconds″"
  }

  class Backend($ : BackendScope[Props, State]) {
    def toggleSeriesVisibility(series: ElevationSeries): Callback =
      $.modStateL(State.shownSeries)(shownSeries =>
        if (shownSeries.contains(series)) shownSeries - series else shownSeries + series
      )

    def render(props: Props, state: State) = {
      val observingNight  = ObservingNight.fromSiteAndLocalDate(props.site, props.date)
      val tbOfficialNight = observingNight.twilightBoundedUnsafe(TwilightType.Official)
      val tbNauticalNight = observingNight.twilightBoundedUnsafe(TwilightType.Nautical)

      val start          = tbOfficialNight.start
      val end            = tbOfficialNight.end
      val skyCalcResults =
        SkyCalc.forInterval(props.site, start, end, PlotEvery, _ => props.coords)
      val series         = skyCalcResults
        .map { case (instant, results) =>
          val millisSinceEpoch = instant.toEpochMilli.toDouble

          def point(value: Double): Chart.Data =
            PointOptionsObject(js.undefined)
              .setX(millisSinceEpoch)
              .setY(value)

          point(results.altitude.toAngle.toSignedDoubleDegrees) ::
            point(-results.totalSkyBrightness) ::
            point(results.parallacticAngle.toSignedDoubleDegrees) ::
            point(results.lunarElevation.toAngle.toSignedDoubleDegrees) ::
            HNil
        }

      val seriesData = seriesDataGen.from(series.unzipN)

      def timeFormat(value: Double): String =
        ZonedDateTime
          .ofInstant(Instant.ofEpochMilli(value.toLong), props.zoneId)
          .format(dateTimeFormatter)

      val timeZone: String =
        props.zoneId match {
          case ZoneOffset.UTC => "UTC"
          case other          => other.getId
        }

      val tickFormatter: AxisLabelsFormatterCallbackFunction =
        (
          labelValue: AxisLabelsFormatterContextObject[Double],
          _:          AxisLabelsFormatterContextObject[String]
        ) => timeFormat(labelValue.value)

      val tooltipFormatter: TooltipFormatterCallbackFunction = {
        (ctx: TooltipFormatterContextObject, _: Tooltip) =>
          val time  = timeFormat(ctx.x)
          val value = ctx.series.index match {
            case 2 => // Sky Brightness
              "%0.2f".format(ctx.y)
            case _ => formatAngle(ctx.y)
          }
          s"<strong>$time ($timeZone)</strong><br/>${ctx.series.name}: $value"
      }

      val sunset  = ZonedDateTime
        .ofInstant(tbNauticalNight.start, props.zoneId)
        .format(dateTimeFormatter)
      val sunrise = ZonedDateTime
        .ofInstant(tbNauticalNight.end, props.zoneId)
        .format(dateTimeFormatter)

      val options = Options()
        .setChart(ChartOptions().setHeight(props.height).setStyledMode(true).setAlignTicks(false))
        .setTitle(
          TitleOptions().setText(
            s"Sunset ${observingNight.toLocalDate.minusDays(1)} ⟶ Sunrise ${observingNight.toLocalDate} "
          )
        )
        .setCredits(CreditsOptions().setEnabled(false))
        .setTooltip(TooltipOptions().setFormatter(tooltipFormatter))
        .setXAxis(
          XAxisOptions()
            .setType(AxisTypeValue.datetime)
            .setLabels(XAxisLabelsOptions().setFormatter(tickFormatter))
            .setTickInterval(MillisPerHour)
            .setMinorTickInterval(MillisPerHour / 2)
            .setPlotBands(
              List(
                XAxisPlotBandsOptions()
                  .setFrom(tbNauticalNight.start.toEpochMilli.toDouble)
                  .setTo(tbNauticalNight.end.toEpochMilli.toDouble)
                  .setClassName("plot-band-twilight-nautical")
                  .setLabel(
                    XAxisPlotBandsLabelOptions()
                      .setText(s"Evening 12° - Twilight: $sunset")
                      .setRotation(270)
                      .setAlign(AlignValue.left)
                      .setTextAlign(AlignValue.center)
                      .setVerticalAlign(VerticalAlignValue.middle)
                  ),
                XAxisPlotBandsOptions() // Empty band, just to draw the label
                  .setFrom(tbNauticalNight.end.toEpochMilli.toDouble)
                  .setTo(tbNauticalNight.end.toEpochMilli.toDouble)
                  .setClassName("plot-band-twilight-nautical-end")
                  .setLabel(
                    XAxisPlotBandsLabelOptions()
                      .setText(s"Evening 12° - Twilight: $sunrise")
                      .setRotation(270)
                      .setAlign(AlignValue.left)
                      .setTextAlign(AlignValue.center)
                      .setVerticalAlign(VerticalAlignValue.middle)
                  )
              ).toJSArray
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
              .setTitle(YAxisTitleOptions().setText("Brightness (mags/arcsec²)"))
              .setMin(-22)
              .setMax(-17)
              .setTickInterval(1)
              .setClassName("plot-axis-sky-brightness")
              .setShowEmpty(false)
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
          ElevationSeries.all
            .map(series =>
              SeriesLineOptions(line)
                .setName(series.name)
                .setYAxis(series.yAxis)
                .setData(series.data(seriesData).toJSArray)
                .setVisible(state.shownSeries.contains(series))
                .setEvents(
                  SeriesEventsOptionsObject()
                    .setLegendItemClick((_: Series, _: SeriesLegendItemClickEventObject) =>
                      toggleSeriesVisibility(series).runNow()
                    )
                )
            )
            .map(_.asInstanceOf[SeriesOptionsType])
            .toJSArray
        )

      val (moonPhase, moonIllum) = skyCalcResults(
        skyCalcResults.length / 2
      ).bimap(MoonCalc.approxPhase, _.lunarIlluminatedFraction.toDouble)

      <.span(
        <.div(ExploreStyles.MoonPhase)(
          <.span(
            MoonPhase(phase = moonPhase, size = 20, border = "1px solid black"),
            <.small("%1.0f%%".format(moonIllum * 100))
          )
        ),
        Chart(options).withKey(props.toString)
      )
    }
  }

  val component =
    ScalaComponent
      .builder[Props]
      .initialState(State())
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build
}
