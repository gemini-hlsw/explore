// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import eu.timepit.refined.types.numeric.NonNegInt
import explore.components.ui.ExploreStyles
import explore.model.itc.ItcChartExposureTime
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

case class ItcSpectroscopyPlotDescription(exposureTime: Option[ItcChartExposureTime])
    extends ReactFnProps[ItcSpectroscopyPlotDescription](ItcSpectroscopyPlotDescription.component)

object ItcSpectroscopyPlotDescription {
  type Props = ItcSpectroscopyPlotDescription

  def format(time: NonNegDuration, count: NonNegInt): String =
    s"$count × ${formatDuration(time.value.getSeconds())} = ${formatDuration(time.value.getSeconds() * count.value)}"

  val component = ScalaFnComponent
    .withHooks[Props]
    .render { props =>
      Form(size = Small, clazz = ExploreStyles.ItcPlotDescription)(
        <.label("Integration Time:"),
        <.span(
          props.exposureTime.fold("-") {
            case ItcChartExposureTime(true, time, count) =>
              s"${format(time, count)} *"
            case ItcChartExposureTime(_, time, count)    =>
              format(time, count)
          }
        )
      )
    }
}
