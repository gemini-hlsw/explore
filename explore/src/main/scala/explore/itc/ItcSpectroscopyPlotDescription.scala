// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.data.NonEmptyList
import crystal.Pot
import eu.timepit.refined.types.numeric.NonNegInt
import explore.components.ui.ExploreStyles
import explore.model.itc.ItcCcd
import explore.model.itc.ItcChartExposureTime
import explore.model.itc.OverridenExposureTime
import explore.model.itc.math.*
import explore.syntax.ui.*
import explore.syntax.ui.given
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.NonNegDuration
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.semanticui.collections.form.Form
import react.semanticui.sizes._

case class ItcSpectroscopyPlotDescription(
  exposureTime: Option[ItcChartExposureTime],
  ccds:         Pot[NonEmptyList[ItcCcd]]
) extends ReactFnProps[ItcSpectroscopyPlotDescription](ItcSpectroscopyPlotDescription.component)

object ItcSpectroscopyPlotDescription {
  type Props = ItcSpectroscopyPlotDescription

  def format(time: NonNegDuration, count: NonNegInt): String =
    s"$count × ${formatDuration(time.value.getSeconds())} = ${formatDuration(time.value.getSeconds() * count.value)}"

  def formatCcds(ccds: Pot[NonEmptyList[ItcCcd]], extractor: NonEmptyList[ItcCcd] => String) =
    ccds.toOption.fold("-")(extractor)

  val component = ScalaFnComponent[Props] { props =>
    Form(size = Small, clazz = ExploreStyles.ItcPlotDescription)(
      <.label("Integration Time:"),
      <.span(
        props.exposureTime.fold("-") { case ItcChartExposureTime(_, time, count) =>
          format(time, count)
        }
      ),
      <.label("S/N per exposure:"),
      <.span(formatCcds(props.ccds, _.maxSingleSNRatio.toString)),
      <.label("Total S/N:"),
      <.span(formatCcds(props.ccds, _.maxTotalSNRatio.toString)),
      <.label("Peak (signal + background):"),
      <.span(formatCcds(props.ccds, ccds => s"${ccds.maxPeakPixelFlux} e⁻ (${ccds.maxADU} ADU)"))
    )
  }
}
