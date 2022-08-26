// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.highcharts

import cats.syntax.all.*
import gpp.highcharts.anon.TypeofHighcharts
import gpp.highcharts.anon.TypeofHighchartsAST
import gpp.highcharts.mod.Chart_
import gpp.highcharts.mod.HTMLDOMElement
import gpp.highcharts.mod.Options
import gpp.highcharts.mod.PointOptionsObject
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.syntax.all.given
import org.scalajs.dom.html
import react.common.ReactFnProps
import react.common.ReactProps
import react.common.style.Css
import react.resizeDetector.UseResizeDetectorProps
import react.resizeDetector.hooks.*

import scala.scalajs.js
import scala.scalajs.js.|

case class ResizingChart(
  options:    Options,
  onCreate:   Chart_ => Callback = _ => Callback.empty,
  wrapperCss: Css = Css.Empty,
  highcharts: TypeofHighchartsAST = Highcharts
) extends ReactFnProps(ResizingChart.component)

object ResizingChart {
  type Props = ResizingChart

  type Data =
    Double |
      scala.scalajs.js.Tuple2[
        Double | String,
        Double | Null
      ] | Null | PointOptionsObject

  val component =
    ScalaFnComponent
      .withHooks[Props]
      // ref to the chart div
      .useRefToVdom[html.Element]
      // ref to the chart
      .useRef(none[Chart_])
      // Build the chart and setuup the destroy
      .useEffectOnMountBy((props, containerRef, graphRef) =>
        containerRef.foreach { element =>
          props.highcharts.chart(
            element.asInstanceOf[HTMLDOMElement],
            props.options,
            c => (graphRef.set(Some(c)) *> props.onCreate(c)).runNow()
          )
          ()
        } *>
          // We need to destroy at unmount or we'll leak memory
          CallbackTo(graphRef.foreach(_.foreach(_.destroy())))
      )
      // On resize do a reflow
      .useResizeDetectorBy((_, _, graphRef) =>
        // Reflow on resize to adapt to the size
        UseResizeDetectorProps(onResize = (_, _) => graphRef.foreach(_.foreach(c => c.reflow())))
      )
      .render { (props, containerRef, _, resize) =>
        // Unfortunately we need two divs to have two references setup
        <.div(props.wrapperCss, <.div().withRef(containerRef)).withRef(resize.ref)
      }
}

case class Chart(
  options:    Options,
  onCreate:   Chart_ => Callback = _ => Callback.empty,
  highcharts: TypeofHighchartsAST = Highcharts
) extends ReactProps(Chart.component)

object Chart {
  type Props = Chart

  type Data =
    Double |
      scala.scalajs.js.Tuple2[
        Double | String,
        Double | Null
      ] | Null | PointOptionsObject

  class Backend($ : BackendScope[Props, Unit]) {
    private val containerRef = Ref[html.Element]
    private val graphRef     = Ref[Chart_]

    def render(props: Props) =
      <.div.withRef(containerRef)

    def destroy(): Callback =
      graphRef.foreach(_.destroy())

    def refresh(props: Props): Callback =
      containerRef.foreach { element =>
        val result = props.highcharts.chart(
          element.asInstanceOf[HTMLDOMElement],
          props.options,
          c => (props.onCreate(c) *> graphRef.set(Some(c))).runNow()
        )
        ()
      }
  }

  // We are purposefully not updating the chart on each rerender.
  // To update the chart either:
  //  A) Call the refresh method via a Ref; or
  //  B) Remount with a different key.
  val component =
    ScalaComponent
      .builder[Props]
      .renderBackend[Backend]
      .componentDidMount($ => $.backend.refresh($.props))
      .componentWillUnmount($ => $.backend.destroy())
      .build
}
