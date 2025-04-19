// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import lucuma.react.common.style.Css
import lucuma.ui.syntax.all.given

case class WIP(clazz: Css, elem: VdomNode) extends ReactFnProps[WIP](WIP.component)

object WIP {
  type Props = WIP

  def apply(elem: VdomNode): WIP = WIP(Css.Empty, elem)

  val component = ScalaFnComponent[Props](p =>
    <.div(ExploreStyles.WIP, p.clazz)(
      <.span(ExploreStyles.WIPWarning, "WORK IN PROGRESS"),
      p.elem
    )
  )
}
