package explore.components

import react.common.ReactProps
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.VdomNode
import explore.components.ui.ExploreStyles

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
