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
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.primereact.Button
import react.primereact.SelectButton
import react.primereact.SelectItem

case class ItcPlotControl(
  chartType:   ViewOpt[ItcChartType],
  showDetails: ViewOpt[PlotDetails]
) extends ReactFnProps[ItcPlotControl](ItcPlotControl.component)

object ItcPlotControl:
  private type Props = ItcPlotControl

  private val component = ScalaFnComponent[Props] { props =>
    val descText                = if (props.showDetails.get.exists(_.value)) "Hide details" else "Show details"
    def label(ch: ItcChartType) = ch match
      case ItcChartType.S2NChart    => "S/N"
      case ItcChartType.SignalChart => "Signal"

    val options = ItcChartType.values
      .map(ch =>
        SelectItem(ch, label = label(ch), clazz = LucumaStyles.Tiny |+| LucumaStyles.Compact)
      )
      .toList
      .reverse

    <.div(
      ExploreStyles.ItcPlotControls,
      Button(
        onClick = props.showDetails.mod {
          case PlotDetails.Shown  => PlotDetails.Hidden
          case PlotDetails.Hidden => PlotDetails.Shown
        },
        label = descText
      ).tiny.compact,
      props.chartType.asView.map { ct =>
        SelectButton[ItcChartType](ct.get,
                                   options,
                                   clazz = LucumaStyles.Tiny,
                                   onChange = props.chartType.set
        )
      }
    )
  }
