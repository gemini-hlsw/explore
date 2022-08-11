// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.syntax.all._
import crystal.Pot
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.itc.ItcChart
import explore.syntax.ui.*
import explore.syntax.ui.given
import gpp.highcharts.highchartsStrings.line
import gpp.highcharts.mod.XAxisLabelsOptions
import gpp.highcharts.mod._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.util.Enumerated
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.highcharts.Chart
import react.resizeDetector.hooks._

import scala.collection.immutable.HashSet
import scala.scalajs.js
import scala.scalajs.js.JSConverters._

final case class ItcSpectroscopyPlot(charts: Pot[List[ItcChart]])
    extends ReactFnProps[ItcSpectroscopyPlot](ItcSpectroscopyPlot.component)

object ItcSpectroscopyPlot {
  type Props = ItcSpectroscopyPlot

  final case class ItcSeries(name: String, yAxis: Int) extends Product with Serializable

  val component = ScalaFnComponent
    .withHooks[Props]
    .useResizeDetector()
    .render { (props, resize) =>
      val seriesData: List[js.Array[Chart.Data]] =
        props.charts.toOption.foldMap(
          _.map(_.data.map(p => (p(0), p(1)): Chart.Data).toJSArray)
        )

      val series =
        props.charts.toOption.foldMap(_.map(chart => ItcSeries(chart.title, 0)))

      val yAxes = props.charts.toOption
        .foldMap(
          _.map(chart =>
            YAxisOptions()
              .setTitle(YAxisTitleOptions().setText(chart.title))
              .setAllowDecimals(false)
              .setTickInterval(10)
              .setMinorTickInterval(5)
              .setLabels(YAxisLabelsOptions().setFormat("{value}"))
          )
        )
        .headOption
        .toList

      val options = Options()
        .setChart(
          ChartOptions()
            .setHeight(resize.height.getOrElse(1).toDouble)
            .setStyledMode(true)
            .setAlignTicks(false)
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
          series.zipWithIndex
            .map((series, i) =>
              SeriesLineOptions((), (), line)
                .setName(series.name)
                .setYAxis(series.yAxis)
                .setData(seriesData.lift(i).getOrElse(js.Array()))
                .setLineWidth(0.1)
            )
            .map(_.asInstanceOf[SeriesOptionsType])
            .toJSArray
        )

      <.div(
        ExploreStyles.ElevationPlotSection,
        // Include the size in the key
        Chart(options).withKey(s"$props-$resize").when(resize.height.isDefined)
      )
        .withRef(resize.ref)
    }
}
