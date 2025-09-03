// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.data.NonEmptyChain
import eu.timepit.refined.types.numeric.PosInt
import explore.components.ui.ExploreStyles
import explore.model.itc.ItcExposureTime
import explore.model.itc.math.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.itc.ItcCcd
import lucuma.itc.SingleSN
import lucuma.itc.TotalSN
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*

case class ItcSpectroscopyPlotDescription(
  brightness:   Option[BrightnessValues],
  exposureTime: ItcExposureTime,
  ccds:         NonEmptyChain[ItcCcd],
  finalSN:      TotalSN,
  singleSN:     SingleSN
) extends ReactFnProps[ItcSpectroscopyPlotDescription](ItcSpectroscopyPlotDescription)

object ItcSpectroscopyPlotDescription
    extends ReactFnComponent[ItcSpectroscopyPlotDescription](props =>
      val finalSN: String    = formatSN(props.finalSN.value)
      val singleSN: String   = formatSN(props.singleSN.value)
      val brightness: String = props.brightness.fold("-")(_.toString)

      val exposureTime: String =
        // Not ideal, it needs a fix on lucuma-ui
        format(props.exposureTime.time, PosInt.unsafeFrom(props.exposureTime.count.value))

      val ccds: String                         = s"${props.ccds.maxPeakPixelFlux} 𝐞⁻ (${props.ccds.maxADU} ADU)"
      val warningsWithCcd: List[(Int, String)] =
        props.ccds.toNonEmptyList.toList.zipWithIndex.flatMap { case (ccd, index) =>
          ccd.warnings.map(w => (index + 1, w.msg))
        }.distinct

      <.div(
        ExploreStyles.ItcPlotDescription,
        <.label("Integration Time:"),
        <.span(exposureTime),
        <.label("S/N per exposure:"),
        <.span(singleSN),
        <.label("Total S/N:"),
        <.span(finalSN),
        <.label("Peak (signal + background):"),
        <.span(ccds),
        <.label("Input brightness:"),
        <.span(brightness),
        <.label(ExploreStyles.ItcPlotDescriptionWarnings, "Warnings:")
          .when(warningsWithCcd.nonEmpty),
        <.span(
          ExploreStyles.ItcPlotDescriptionWarnings,
          warningsWithCcd.map { case (ccdIndex, msg) =>
            <.div(ExploreStyles.WarningIcon, s"CCD $ccdIndex: $msg")
          }.toReactFragment
        )
      )
    )
