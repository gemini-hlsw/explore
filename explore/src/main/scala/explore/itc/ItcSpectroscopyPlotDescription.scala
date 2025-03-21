// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.data.NonEmptyChain
import eu.timepit.refined.types.numeric.PosInt
import explore.components.ui.ExploreStyles
import explore.model.itc.ItcExposureTime
import explore.model.itc.math.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.SignalToNoise
import lucuma.itc.ItcCcd
import lucuma.itc.SingleSN
import lucuma.itc.TotalSN
import lucuma.react.common.ReactFnProps
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*

case class ItcSpectroscopyPlotDescription(
  brightness:   Option[BrightnessValues],
  exposureTime: Option[ItcExposureTime],
  ccds:         Option[NonEmptyChain[ItcCcd]],
  finalSN:      Option[TotalSN],
  singleSN:     Option[SingleSN]
) extends ReactFnProps[ItcSpectroscopyPlotDescription](ItcSpectroscopyPlotDescription.component)

object ItcSpectroscopyPlotDescription {
  type Props = ItcSpectroscopyPlotDescription

  val component = ScalaFnComponent[Props] { props =>
    val finalSN: String    = props.finalSN.fold("-")(u => formatSN(u.value))
    val singleSN: String   = props.singleSN.fold("-")(u => formatSN(u.value))
    val brightness: String = props.brightness.fold("-")(_.toString)

    <.div(
      ExploreStyles.ItcPlotDescription,
      <.label("Integration Time:"),
      <.span(props.exposureTime.fold("-") { case ItcExposureTime(time, count) =>
        // Not ideal, it needs a fix on lucuma-ui
        format(time, PosInt.unsafeFrom(count.value))
      }),
      <.label("S/N per exposure:"),
      <.span(singleSN),
      <.label("Total S/N:"),
      <.span(finalSN),
      <.label("Peak (signal + background):"),
      <.span(formatCcds(props.ccds, ccds => s"${ccds.maxPeakPixelFlux} 𝐞⁻ (${ccds.maxADU} ADU)")),
      <.label("Input brightness:"),
      <.span(brightness)
    )
  }
}
