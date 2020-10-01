// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import explore.components.ui.ExploreStyles
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common.ReactProps

final case class WIP(elem: VdomNode) extends ReactProps[WIP](WIP.component)

object WIP {
  type Props = WIP

  val component = ScalaComponent
    .builder[Props]
    .render_P(p =>
      <.div(ExploreStyles.WIP)(
        <.span(ExploreStyles.WIPWarning, "WORK IN PROGRESS"),
        p.elem
      )
    )
    .build
}
