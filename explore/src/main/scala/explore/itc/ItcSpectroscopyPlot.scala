// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.data.NonEmptyChain
import explore.components.ui.ExploreStyles
import cats.syntax.all.*
import explore.highcharts.*
import explore.model.itc.*
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Wavelength
import lucuma.itc.GraphType
import lucuma.itc.ItcCcd
import lucuma.itc.client.GraphResult
import lucuma.itc.math.roundToSignificantFigures
import lucuma.react.common.ReactFnProps
import lucuma.react.highcharts.Chart
import lucuma.typed.highcharts.mod.{^ as _, *}
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*

case class ItcSpectroscopyPlot(
  ccds:            NonEmptyChain[ItcCcd],
  graphs:          NonEmptyChain[GraphResult],
  graphType:       GraphType,
  targetName:      String,
  signalToNoiseAt: Wavelength,
  details:         PlotDetails
) extends ReactFnProps(ItcSpectroscopyPlot.component)

object ItcSpectroscopyPlot {
  private def chartOptions(
    graph:           GraphResult,
    targetName:      String,
    signalToNoiseAt: Wavelength
  ) = {
    val yAxis            = graph.series.foldLeft(YAxis.Empty)(_ ∪ _.yAxis.yAxis)
    val title            = graph.graphType match
      case GraphType.SignalGraph      => "𝐞⁻ per exposure per spectral pixel"
      case GraphType.S2NGraph         => "S/N per spectral pixel"
      case GraphType.SignalPixelGraph => "S/N per pixel"
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

    val graphClassName = graph.graphType.toString.toLowerCase()

    def rounded(x: js.UndefOr[Double | String]): String =
      x.toOption.fold("-") {
        case x: Double => roundToSignificantFigures(x, 4).toString
        case x: String => x
      }

    val tooltipFormatter: TooltipFormatterCallbackFunction =
      (ctx: TooltipFormatterContextObject, _: Tooltip) =>
        val x        = rounded(ctx.x)
        val y        = rounded(ctx.y)
        val measUnit = if (graph.graphType === GraphType.SignalGraph) " 𝐞⁻" else ""
        s"""<strong>$x nm</strong><br/><span class="$graphClassName highcharts-color-${ctx.colorIndex.toInt}">●</span> ${ctx.series.name}: <strong>$y$measUnit</strong>"""

    val graphTitle = graph.graphType match
      case GraphType.SignalGraph      => "Signal in 1-pixel"
      case GraphType.S2NGraph         => "Signal / Noise"
      case GraphType.SignalPixelGraph => "Pixel"

    val plotLines = graph.graphType match
      case GraphType.SignalGraph      => js.Array()
      case GraphType.SignalPixelGraph => js.Array()
      case GraphType.S2NGraph         =>
        val value = signalToNoiseAt.toNanometers.value.value.toDouble
        List(
          XAxisPlotLinesOptions()
            .setDashStyle(DashStyleValue.LongDash)
            .setWidth(3)
            .setValue(value)
            .clazz(ExploreStyles.ItcPlotWvPlotLine)
            .setZIndex(10)
            .setLabel(XAxisPlotLinesLabelOptions().setText(f"$value%.1f nm"))
        ).toJSArray

    Options()
      .setChart(
        commonOptions.clazz(ExploreStyles.ItcPlotChart)
      )
      .setTitle(TitleOptions().setText(graphTitle))
      .setSubtitle(
        SubtitleOptions().setText(targetName)
      )
      .setCredits(CreditsOptions().setEnabled(false))
      .setLegend(LegendOptions().setMargin(0))
      .setTooltip(TooltipOptions().setFormatter(tooltipFormatter).setClassName(graphClassName))
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
        graph.series
          .map: series =>
            SeriesLineOptions((), ())
              .setName(series.title)
              .setYAxis(0)
              .setData(
                series.data
                  .map(p => (p(0), p(1)): Chart.Data)
                  .toJSArray
              )
              .setClassName(graphClassName)
              .setLineWidth(1)
          .map(_.asInstanceOf[SeriesOptionsType])
          .toJSArray
      )
  }

  private val EmptyGraphOptions: Reusable[Options] =
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
            .clazz(ExploreStyles.ItcPlotChart)
            // Will be used in the future to persist the zoom
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

  private val component = ScalaFnComponent[ItcSpectroscopyPlot]: props =>
    for {
      itcGraphOptions <- useMemo((props.graphs, props.targetName, props.signalToNoiseAt)):
                           (graphs, targetName, signalToNoiseAt) =>
                             graphs.toList
                               .map: graph =>
                                 graph.graphType -> chartOptions(
                                   graph,
                                   targetName,
                                   signalToNoiseAt
                                 )
                               .toMap
      options         <- useMemo((props.graphType, itcGraphOptions)): (graphType, itcGraphOptions) =>
                           itcGraphOptions.get(graphType)
    } yield
      val chartOptions: Reusable[Options] = options.sequenceOption.getOrElse(EmptyGraphOptions)

      Chart(
        chartOptions,
        allowUpdate = false,
        containerMod = TagMod(ExploreStyles.ItcPlotBody)
      )
}
