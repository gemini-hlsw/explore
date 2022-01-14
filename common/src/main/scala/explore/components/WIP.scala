// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import explore.components.ui.ExploreStyles
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common.ReactProps
import react.common.style.Css

final case class WIP(clazz: Css, elem: VdomNode) extends ReactProps[WIP](WIP.component)

object WIP {
  type Props = WIP

  def apply(elem: VdomNode): WIP = WIP(Css.Empty, elem)

  val component = ScalaComponent
    .builder[Props]
    .render_P(p =>
      <.div(ExploreStyles.WIP, p.clazz)(
        <.span(ExploreStyles.WIPWarning, "WORK IN PROGRESS"),
        p.elem
      )
    )
    .build
}
