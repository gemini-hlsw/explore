// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.syntax.all.*
import crystal.react.ViewOpt
import explore.components.ui.ExploreStyles
import explore.model.enums.ItcChartType
import explore.model.itc.PlotDetails
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.ButtonGroup
import react.semanticui.sizes.*

case class ItcPlotControl(
  chartType:   ViewOpt[ItcChartType],
  showDetails: ViewOpt[PlotDetails]
) extends ReactFnProps[ItcPlotControl](ItcPlotControl.component)

object ItcPlotControl:
  type Props = ItcPlotControl

  val component = ScalaFnComponent
    .withHooks[Props]
    .render { props =>
      val descText = if (props.showDetails.get.exists(_.value)) "Hide details" else "Show details"

      <.div(
        ExploreStyles.ItcPlotControls,
        Button(
          compact = true,
          size = Tiny,
          active = true,
          onClick = props.showDetails.mod {
            case PlotDetails.Shown  => PlotDetails.Hidden
            case PlotDetails.Hidden => PlotDetails.Shown
          }
        )(descText),
        ButtonGroup(compact = true, size = Tiny, clazz = ExploreStyles.ItcPlotSelector)(
          Button(
            active = props.chartType.get.exists(_ === ItcChartType.S2NChart),
            onClick = props.chartType.set(ItcChartType.S2NChart)
          )("S/N"),
          Button(
            active = props.chartType.get.exists(_ === ItcChartType.SignalChart),
            onClick = props.chartType.set(ItcChartType.SignalChart)
          )("Signal")
        )
      )
    }
