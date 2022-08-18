// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.syntax.all._
import crystal.Pot
import explore.components.ui.ExploreStyles
import explore.highcharts.*
import explore.implicits._
import explore.model.itc.ItcChart
import explore.model.itc.YAxis
import explore.syntax.ui.*
import explore.syntax.ui.given
import explore.utils.*
import gpp.highcharts.highchartsStrings.line
import gpp.highcharts.mod._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.util.Enumerated
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.highcharts.Chart
import react.resizeDetector.hooks._
import react.semanticui.elements.loader.Loader

import scala.collection.immutable.HashSet
import scala.scalajs.js
import scala.scalajs.js.JSConverters._

case class ItcSpectroscopyPlot(loading: PlotLoading, charts: Pot[List[ItcChart]])
    extends ReactFnProps[ItcSpectroscopyPlot](ItcSpectroscopyPlot.component)

object ItcSpectroscopyPlot {
  type Props = ItcSpectroscopyPlot

  val component = ScalaFnComponent
    .withHooks[Props]
    .useResizeDetector()
    .render { (props, resize) =>
      val loading = props.charts.isPending || props.loading.boolValue

      val series =
        props.charts.toOption.filterNot(_ => loading).orEmpty

      val yAxes = props.charts.toOption
        .map(_.foldLeft(YAxis.Empty)(_ ∪ _.yAxis))
        .map { yAxis =>
          val (min, max, tick) = yAxis.ticks(10)
          YAxisOptions()
            .setTitle(YAxisTitleOptions().setText("e- per exposure per spectral pixel"))
            .setAllowDecimals(false)
            .setTickInterval(tick)
            .setMin(min)
            .setMax(max)
            .setMinorTickInterval(tick / 3)
            .setLabels(YAxisLabelsOptions().setFormat("{value}"))
        }
        .toList

      val options = Options()
        .setChart(
          ChartOptions()
            .setHeight(resize.height.getOrElse(1).toDouble)
            .setStyledMode(true)
            .setAlignTicks(false)
            .clazz(
              ExploreStyles.ItcPlotChart |+|
                ExploreStyles.ItcPlotLoading.when_(props.loading.boolValue)
            )
            .setZoomType(OptionsZoomTypeValue.xy)
            .setPanning(ChartPanningOptions().setEnabled(true))
            .setPanKey(OptionsPanKeyValue.shift)
            .setAnimation(false)
            // Will be used in the future to persist the soom
            // .selectionCB(s => Callback.log(s"selection ${s.xAxis(0).min}"))
        )
        .setTitle(TitleOptions().setTextUndefined)
        .setCredits(CreditsOptions().setEnabled(false))
        .setXAxis(
          XAxisOptions()
            .setType(AxisTypeValue.linear)
        )
        .setYAxis(yAxes.toJSArray)
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
          series
            .map(series =>
              SeriesLineOptions((), (), line)
                .setName(series.title)
                .setYAxis(0)
                .setData(series.data.map(p => (p(0), p(1)): Chart.Data).toJSArray)
                .setLineWidth(0.1)
            )
            .map(_.asInstanceOf[SeriesOptionsType])
            .toJSArray
        )

      <.div(
        ExploreStyles.ItcPlotSection,
        Chart(options, onCreate = _.showLoadingCB.when_(loading))
          .withKey(s"$props-$resize")
          .when(resize.height.isDefined)
      )
        .withRef(resize.ref)
    }
}
