// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.syntax.all.*
import crystal.react.View
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.itc.PlotDetails
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.itc.ChartType
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.SelectButtonEnumView
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import monocle.Prism

case class ItcPlotControl(
  chartType:   View[ChartType],
  showDetails: View[PlotDetails]
) extends ReactFnProps[ItcPlotControl](ItcPlotControl.component)

enum AllowedChartType(val tag: String) derives Enumerated:
  case S2N    extends AllowedChartType("sn")
  case Signal extends AllowedChartType("signal")

object ItcPlotControl:
  private type Props = ItcPlotControl

  private given Display[AllowedChartType] = Display.byShortName {
    case AllowedChartType.S2N    => "S/N"
    case AllowedChartType.Signal => "Signal"
  }

  private val typePrism: Prism[ChartType, AllowedChartType] = Prism[ChartType, AllowedChartType] {
    case ChartType.S2NChart    => Some(AllowedChartType.S2N)
    case ChartType.SignalChart => Some(AllowedChartType.Signal)
    case _                     => None
  } {
    case AllowedChartType.S2N    => ChartType.S2NChart
    case AllowedChartType.Signal => ChartType.SignalChart
  }

  private val component = ScalaFnComponent[Props] { props =>
    val descText     = if (props.showDetails.get.value) "Hide details" else "Show details"
    val allowedChart = props.chartType.zoom(typePrism).asView

    <.div(ExploreStyles.ItcPlotControls)(
      HelpIcon(
        "target/main/itc-spectroscopy-plot.md".refined,
        ExploreStyles.HelpIconFloating |+| ExploreStyles.ItcPlotHelpIcon
      ),
      Button(
        clazz = ExploreStyles.ItcPlotDetailsToggle,
        onClick = props.showDetails.mod {
          case PlotDetails.Shown  => PlotDetails.Hidden
          case PlotDetails.Hidden => PlotDetails.Shown
        },
        label = descText
      ).tiny.compact,
      allowedChart.map { ct =>
        SelectButtonEnumView(
          "itc-plot-type".refined,
          ct,
          buttonClass = LucumaPrimeStyles.Tiny |+| LucumaPrimeStyles.VeryCompact
        )
      }
    )
  }
