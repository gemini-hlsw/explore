// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.data.NonEmptyList
import cats.syntax.all.*
import crystal.react.View
import eu.timepit.refined.types.numeric.NonNegInt
import explore.components.ui.ExploreStyles
import explore.model.itc.ItcCcd
import explore.model.itc.ItcChartExposureTime
import explore.model.itc.ItcTarget
import explore.model.itc.OverridenExposureTime
import explore.model.itc.math.*
import explore.syntax.ui.*
import explore.syntax.ui.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.NonNegDuration
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.ReactFnProps
import react.semanticui.addons.select.Select
import react.semanticui.addons.select.Select.SelectItem
import react.semanticui.collections.form.Form
import react.semanticui.modules.dropdown.Dropdown.DropdownProps
import react.semanticui.sizes._
import react.semanticui.sizes.*

import scala.scalajs.js.JSConverters._
case class ItcSpectroscopyPlotDescription(
  selectedTarget: View[Option[ItcTarget]],
  targets:        List[ItcTarget],
  exposureTime:   Option[ItcChartExposureTime],
  ccds:           Option[NonEmptyList[ItcCcd]]
) extends ReactFnProps[ItcSpectroscopyPlotDescription](ItcSpectroscopyPlotDescription.component)

object ItcSpectroscopyPlotDescription {
  type Props = ItcSpectroscopyPlotDescription

  def format(time: NonNegDuration, count: NonNegInt): String =
    s"$count × ${formatDuration(time.value.getSeconds())} = ${formatDuration(time.value.getSeconds() * count.value)}"

  def formatCcds(ccds: Option[NonEmptyList[ItcCcd]], extractor: NonEmptyList[ItcCcd] => String) =
    ccds.fold("-")(extractor)

  val component = ScalaFnComponent[Props] { props =>
    def newSelected(p: DropdownProps): Option[ItcTarget] =
      props.targets.find(t => p.value.toOption.exists(_.toString === t.name.value))

    val selected = props.selectedTarget.get.map(_.name.value)

    Form(size = Small, clazz = ExploreStyles.ItcPlotDescription)(
      <.label("Target"),
      if (props.targets.size < 2)
        <.span(props.targets.headOption.map(_.name.value).getOrElse("-"))
      else
        Select(
          compact = true,
          value = selected.orUndefined,
          onChange = e => props.selectedTarget.set(newSelected(e)),
          options = props.targets.map(t =>
            new SelectItem(text = t.name.value,
                           value = t.name.value,
                           selected = props.selectedTarget.get.exists(_ === t)
            )
          )
        ),
      <.label("Integration Time:", ExploreStyles.ItcPlotFirstTextRow),
      <.span(ExploreStyles.ItcPlotFirstTextRow,
             props.exposureTime.fold("-") { case ItcChartExposureTime(_, time, count) =>
               format(time, count)
             }
      ),
      <.label("S/N per exposure:"),
      <.span(formatCcds(props.ccds, _.maxSingleSNRatio.toString)),
      <.label("Total S/N:"),
      <.span(formatCcds(props.ccds, _.maxTotalSNRatio.toString)),
      <.label("Peak (signal + background):"),
      <.span(formatCcds(props.ccds, ccds => s"${ccds.maxPeakPixelFlux} 𝐞⁻ (${ccds.maxADU} ADU)"))
    )
  }
}
