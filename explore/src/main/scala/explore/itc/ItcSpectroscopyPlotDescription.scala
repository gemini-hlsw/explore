// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.data.NonEmptyList
import cats.syntax.all.*
import explore.components.ui.ExploreStyles
import explore.model.itc.ItcCcd
import explore.model.itc.ItcChartExposureTime
import explore.model.itc.math.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.semanticui.collections.form.Form
import react.semanticui.sizes.*

case class ItcSpectroscopyPlotDescription(
  exposureTime: Option[ItcChartExposureTime],
  ccds:         Option[NonEmptyList[ItcCcd]]
) extends ReactFnProps[ItcSpectroscopyPlotDescription](ItcSpectroscopyPlotDescription.component)

object ItcSpectroscopyPlotDescription {
  type Props = ItcSpectroscopyPlotDescription

  val component = ScalaFnComponent[Props] { props =>
    Form(size = Small, clazz = ExploreStyles.ItcPlotDescription)(
      <.label("Integration Time:"),
      <.span(props.exposureTime.fold("-") { case ItcChartExposureTime(_, time, count) =>
        format(time, count)
      }),
      <.label("S/N per exposure:"),
      <.span(formatCcds(props.ccds, _.maxSingleSNRatio.toString)),
      <.label("Total S/N:"),
      <.span(formatCcds(props.ccds, _.maxTotalSNRatio.toString)),
      <.label("Peak (signal + background):"),
      <.span(formatCcds(props.ccds, ccds => s"${ccds.maxPeakPixelFlux} 𝐞⁻ (${ccds.maxADU} ADU)"))
    )
  }
}
