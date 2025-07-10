// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.itc.GraphType
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.syntax.all.given
import monocle.Prism

case class ItcImagingPlotControl(
  graphType:   View[GraphType],
  showDetails: View[PlotDetails]
) extends ReactFnProps[ItcImagingPlotControl](ItcImagingPlotControl.component)

enum AllowedImagingGraphType(val tag: String) derives Enumerated:
  case S2NPixel extends AllowedImagingGraphType("snpixel")
  case Signal   extends AllowedImagingGraphType("signal")

object ItcImagingPlotControl:
  private type Props = ItcImagingPlotControl

  private given Display[AllowedImagingGraphType] = Display.byShortName {
    case AllowedImagingGraphType.S2NPixel => "S/N"
    case AllowedImagingGraphType.Signal   => "Signal"
  }

  private val typePrism: Prism[GraphType, AllowedImagingGraphType] = Prism[GraphType, AllowedImagingGraphType] {
    case GraphType.SignalPixelGraph => Some(AllowedImagingGraphType.S2NPixel)
    case GraphType.SignalGraph      => Some(AllowedImagingGraphType.Signal)
    case _                          => None
  } {
    case AllowedImagingGraphType.S2NPixel => GraphType.SignalPixelGraph
    case AllowedImagingGraphType.Signal   => GraphType.SignalGraph
  }

  private val component = ScalaFnComponent[Props] { props =>
    val descText     = if (props.showDetails.get.value) "Hide details" else "Show details"
    val allowedChart = props.graphType.zoom(typePrism).asView

    <.div(ExploreStyles.ItcPlotControls)(
      HelpIcon(
        "target/main/itc-imaging-plot.md".refined,
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
          "itc-imaging-plot-type".refined,
          ct,
          buttonClass = LucumaPrimeStyles.Tiny |+| LucumaPrimeStyles.VeryCompact
        )
      }
    )
  }