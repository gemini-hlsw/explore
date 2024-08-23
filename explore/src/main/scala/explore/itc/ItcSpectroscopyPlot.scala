// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.data.NonEmptyList
import cats.syntax.all.*
import explore.components.ui.ExploreStyles
import explore.highcharts.*
import explore.model.LoadingState
import explore.model.itc.*
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Wavelength
import lucuma.itc.ChartType
import lucuma.itc.ItcCcd
import lucuma.itc.client.OptimizedChartResult
import lucuma.itc.math.roundToSignificantFigures
import lucuma.react.common.ReactFnProps
import lucuma.react.highcharts.Chart
import lucuma.typed.highcharts.mod.{^ as _, *}
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

case class ItcSpectroscopyPlot(
  ccds:            Option[NonEmptyList[ItcCcd]],
  charts:          Option[NonEmptyList[OptimizedChartResult]],
  error:           Option[String],
  chartType:       ChartType,
  targetName:      Option[String],
  signalToNoiseAt: Option[Wavelength],
  loading:         LoadingState,
  details:         PlotDetails
) extends ReactFnProps(ItcSpectroscopyPlot.component)

object ItcSpectroscopyPlot {
  private type Props = ItcSpectroscopyPlot

  private def chartOptions(
    chart:           OptimizedChartResult,
    targetName:      Option[String],
    loading:         LoadingState,
    signalToNoiseAt: Option[Wavelength],
    maxSNWavelength: Option[Wavelength]
  ) = {
    val yAxis            = chart.series.foldLeft(YAxis.Empty)(_ ∪ _.yAxis.yAxis)
    val title            = chart.chartType match
      case ChartType.SignalChart      => "𝐞⁻ per exposure per spectral pixel"
      case ChartType.S2NChart         => "S/N per spectral pixel"
      case ChartType.SignalPixelChart => "S/N per pixel"
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
        val measUnit = if (chart.chartType === ChartType.SignalChart) " 𝐞⁻" else ""
        s"""<strong>$x nm</strong><br/><span class="$chartClassName highcharts-color-${ctx.colorIndex.toInt}">●</span> ${ctx.series.name}: <strong>$y$measUnit</strong>"""

    val chartTitle = chart.chartType match
      case ChartType.SignalChart      => "Signal in 1-pixel"
      case ChartType.S2NChart         => "Signal / Noise"
      case ChartType.SignalPixelChart => "Pixel"

    val plotLines = chart.chartType match
      case ChartType.SignalChart      => js.Array()
      case ChartType.SignalPixelChart => js.Array()
      case ChartType.S2NChart         =>
        signalToNoiseAt
          .orElse(maxSNWavelength)
          .map(_.toNanometers.value.value.toDouble)
          .foldMap: value =>
            List(
              XAxisPlotLinesOptions()
                .setDashStyle(DashStyleValue.LongDash)
                .setWidth(3)
                .setValue(value)
                .clazz(ExploreStyles.ItcPlotWvPlotLine)
                .setZIndex(10)
                .setLabel(XAxisPlotLinesLabelOptions().setText(f"$value%.1f nm"))
            )
          .toJSArray

    Options()
      .setChart(
        commonOptions
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
          .map: series =>
            SeriesLineOptions((), ())
              .setName(series.title)
              .setYAxis(0)
              .setData(
                series.data
                  .map(p => (p(0), p(1)): Chart.Data)
                  .toJSArray
              )
              .setClassName(chartClassName)
              .setLineWidth(1)
          .map(_.asInstanceOf[SeriesOptionsType])
          .toJSArray
      )
  }

  private val EmptyChartOptions: Reusable[Options] =
    Reusable.always {
      val yAxis = YAxisOptions()
        .setAllowDecimals(false)
        .setMin(0)
        .setMax(100)
        .setTickInterval(10)

      Options()
        .setChart(
          ChartOptions()
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
            // .setHeight("100%")
        )
    }

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useMemoBy(props =>
      (props.charts, props.targetName, props.loading, props.signalToNoiseAt, props.ccds)
    ): _ =>
      (charts, targetName, loading, signalToNoiseAt, ccds) =>
        val series: List[OptimizedChartResult] =
          charts.filterNot(_ => loading.value).foldMap(_.toList)

        val maxSNWavelength: Option[Wavelength] =
          ccds
            .foldMap(_.toList)
            .map(c => (c.maxTotalSNRatio, c.wavelengthForMaxTotalSNRatio))
            .maxByOption(_._1)
            .map(_._2)

        series
          .map: chart =>
            chart.chartType -> chartOptions(
              chart,
              targetName,
              loading,
              signalToNoiseAt,
              maxSNWavelength
            )
          .toMap
    .useMemoBy((props, itcChartOptions) => (props.chartType, itcChartOptions)): (_, _) =>
      (chartType, itcChartOptions) => itcChartOptions.get(chartType)
    .render: (props, _, options) =>
      val loading = props.loading.value

      val chartOptions: Reusable[Options] = options.sequenceOption.getOrElse(EmptyChartOptions)

      def formatErrorMessage(c: Chart_): Callback =
        c.showLoadingCB.when_(loading) *>
          props.error
            .map(e => c.showLoadingCB(e).unless_(loading))
            .orEmpty

      Chart(
        chartOptions,
        allowUpdate = false,
        onCreate = formatErrorMessage,
        containerMod = TagMod(ExploreStyles.ItcPlotBody)
      )
}
