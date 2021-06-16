// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import crystal.react.reuse._
import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common.ReactProps
import react.semanticui.modules.modal.Dimmer.Blurring
import react.semanticui.modules.modal.Modal
import react.semanticui.modules.modal.ModalContent
import react.semanticui.shorthand._

case class About(trigger: Reuse[VdomNode], content: Reuse[VdomNode])
    extends ReactProps[About](About.component)

object About {
  type Props = About

  implicit val propsReuse: Reusability[Props] = Reusability.derive

  protected val component = ScalaComponent
    .builder[Props]
    .render_P { props =>
      Modal(
        dimmer = Blurring,
        trigger = props.trigger: VdomNode,
        closeIcon = Icons.Close.clazz(ExploreStyles.ModalCloseButton),
        content = ModalContent(
          <.div(
            Logo(),
            props.content
          )
        )
      )
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
