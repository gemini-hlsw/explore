// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.Pot
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.highcharts.*
import explore.model.LoadingState
import explore.model.enums.ItcChartType
import explore.model.enums.ItcSeriesType
import explore.model.itc.ItcCcd
import explore.model.itc.ItcChart
import explore.model.itc.ItcSeries
import explore.model.itc.PlotDetails
import explore.model.itc.YAxis
import explore.model.itc.math.roundToSignificantFigures
import explore.syntax.ui.*
import explore.syntax.ui.given
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Wavelength
import lucuma.core.util.Enumerated
import lucuma.refined.*
import lucuma.typed.highcharts.highchartsStrings.line
import lucuma.typed.highcharts.mod.DashStyleValue
import lucuma.typed.highcharts.mod.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.highcharts.ResizingChart
import react.resizeDetector.hooks.*

import scala.collection.immutable.HashSet
import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

case class ItcSpectroscopyPlot(
  ccds:            Option[NonEmptyList[ItcCcd]],
  charts:          Option[NonEmptyList[ItcChart]],
  error:           Option[String],
  chartType:       ItcChartType,
  targetName:      Option[String],
  signalToNoiseAt: Option[Wavelength],
  loading:         LoadingState,
  details:         PlotDetails
) extends ReactFnProps(ItcSpectroscopyPlot.component)

object ItcSpectroscopyPlot {
  private type Props = ItcSpectroscopyPlot

  private def chartOptions(
    chart:           ItcChart,
    targetName:      Option[String],
    loading:         LoadingState,
    details:         PlotDetails,
    signalToNoiseAt: Option[Wavelength],
    maxSNWavelength: Option[Wavelength],
    height:          Double
  ) = {
    val yAxis            = chart.series.foldLeft(YAxis.Empty)(_ ∪ _.yAxis)
    val title            = chart.chartType match
      case ItcChartType.SignalChart => "𝐞⁻ per exposure per spectral pixel"
      case ItcChartType.S2NChart    => "S/N per spectral pixel"
    val (min, max, tick) = yAxis.ticks(10)

    val yAxes = YAxisOptions()
      .setTitle(YAxisTitleOptions().setText(title))
      .setAllowDecimals(false)
      .setTickInterval(tick)
      .setMin(min)
      .setMax(max)
      .setMinorTickInterval(tick / 3)
      .setLineWidth(1)
      .setLabels(YAxisLabelsOptions().setFormat("{value}"))

    val chartClassName = chart.chartType.toString.toLowerCase()

    def rounded(x: js.UndefOr[Double | String]): String =
      x.toOption.fold("-") {
        case x: Double => roundToSignificantFigures(x, 4).toString
        case x: String => x
      }

    val tooltipFormatter: TooltipFormatterCallbackFunction =
      (ctx: TooltipFormatterContextObject, t: Tooltip) =>
        val x        = rounded(ctx.x)
        val y        = rounded(ctx.y)
        val measUnit = if (chart.chartType === ItcChartType.SignalChart) " 𝐞⁻" else ""
        s"""<strong>$x nm</strong><br/><span class="$chartClassName highcharts-color-${ctx.colorIndex.toInt}">●</span> ${ctx.series.name}: <strong>$y$measUnit</strong>"""

    val chartTitle = chart.chartType match
      case ItcChartType.SignalChart => "Signal in 1-pixel"
      case ItcChartType.S2NChart    => "Signal / Noise"

    val plotLines = chart.chartType match
      case ItcChartType.SignalChart => js.Array()
      case ItcChartType.S2NChart    =>
        val value = signalToNoiseAt.orElse(maxSNWavelength).map(_.toNanometers.value.value.toDouble)

        value
          .foldMap(value =>
            List(
              XAxisPlotLinesOptions()
                .setDashStyle(DashStyleValue.LongDash)
                .setWidth(3)
                .setValue(value)
                .clazz(ExploreStyles.ItcPlotWvPlotLine)
                .setZIndex(10)
                .setLabel(XAxisPlotLinesLabelOptions().setText(f"$value%.1f nm"))
            )
          )
          .toJSArray

    Options()
      .setChart(
        commonOptions
          .setHeight(height)
          .clazz(
            ExploreStyles.ItcPlotChart |+|
              ExploreStyles.ItcPlotLoading.when_(loading.value)
          )
      )
      .setTitle(TitleOptions().setText(chartTitle))
      .setSubtitle(
        targetName.fold(SubtitleOptions().setTextUndefined)(t => SubtitleOptions().setText(t))
      )
      .setCredits(CreditsOptions().setEnabled(false))
      .setLegend(LegendOptions().setMargin(0))
      .setTooltip(TooltipOptions().setFormatter(tooltipFormatter).setClassName(chartClassName))
      .setXAxis(
        XAxisOptions()
          .setType(AxisTypeValue.linear)
          .setTitle(XAxisTitleOptions().setText("Wavelength (nm)"))
          .setPlotLines(plotLines)
      )
      .setYAxis(List(yAxes).toJSArray)
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
        chart.series
          .map(series =>
            SeriesLineOptions((), (), line)
              .setName(series.title)
              .setYAxis(0)
              .setData(
                series.data
                  .map(p => (p(0), p(1)): ResizingChart.Data)
                  .toJSArray
              )
              .setClassName(chartClassName)
              .setLineWidth(1)
          )
          .map(_.asInstanceOf[SeriesOptionsType])
          .toJSArray
      )
  }

  private def emptyChartOptions(height: Double) = {
    val yAxis = YAxisOptions()
      .setAllowDecimals(false)
      .setMin(0)
      .setMax(100)
      .setTickInterval(10)

    Options()
      .setChart(
        ChartOptions()
          .setHeight(height)
          .setStyledMode(true)
          .setAlignTicks(false)
          .clazz(
            ExploreStyles.ItcPlotChart |+| ExploreStyles.ItcPlotLoading
          )
          // Will be used in the future to persist the soom
          // .selectionCB(s => Callback.log(s"selection ${s.xAxis(0).min}"))
      )
      .setTitle(TitleOptions().setTextUndefined)
      .setCredits(CreditsOptions().setEnabled(false))
      .setXAxis(
        XAxisOptions()
          .setType(AxisTypeValue.linear)
      )
      .setYAxis(List(yAxis).toJSArray)
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
  }

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useResizeDetector()
    .render { (props, resize) =>
      val loading = props.loading.value

      val series: List[ItcChart] =
        props.charts.filterNot(_ => loading).foldMap(_.toList)

      val height                              = resize.height.getOrElse(1).toDouble
      val maxSNWavelength: Option[Wavelength] =
        props.ccds
          .foldMap(_.toList)
          .map(c => (c.maxTotalSNRatio, c.wavelengthForMaxTotalSNRatio))
          .maxByOption(_._1)
          .map(_._2)

      val itcChartOptions = series.map { chart =>
        chart.chartType -> chartOptions(chart,
                                        props.targetName,
                                        props.loading,
                                        props.details,
                                        props.signalToNoiseAt,
                                        maxSNWavelength,
                                        height
        )
      }.toMap

      <.div(
        ExploreStyles.ItcPlotBody,
        HelpIcon("target/main/itc-spectroscopy-plot.md".refined, ExploreStyles.HelpIconFloating),
        itcChartOptions
          .get(props.chartType)
          .map { opt =>
            ResizingChart(
              opt,
              onCreate = c =>
                c.reflowCB *>
                  c.showLoadingCB.when_(loading) *>
                  props.error
                    .map(e => c.showLoadingCB(e).unless_(loading))
                    .orEmpty,
              wrapperCss = ExploreStyles.ItcPlotWrapper
            )
              .withKey(s"$props-$resize")
              .when(resize.height.isDefined)
          }
          .getOrElse(
            ResizingChart(emptyChartOptions(height),
                          onCreate = c =>
                            c.showLoadingCB.when_(loading) *>
                              props.error
                                .map(e => c.showLoadingCB(e).unless_(loading))
                                .orEmpty
            )
              .withKey(s"$props-$resize")
              .when(resize.height.isDefined)
          )
      )
        .withRef(resize.ref)
    }
}
