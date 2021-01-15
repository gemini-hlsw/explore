// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import explore.Icons
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common.ReactProps
import react.semanticui.modules.modal.Dimmer.Blurring
import react.semanticui.modules.modal.{ Modal, ModalContent }

case class About(trigger: VdomNode, content: VdomNode) extends ReactProps[About](About.component)

object About {
  type Props = About

  protected val component = ScalaComponent
    .builder[Props]
    .render_P { props =>
      Modal(
        dimmer = Blurring,
        trigger = props.trigger,
        closeIcon = Icons.Close,
        content = ModalContent(
          <.div(
            Logo(),
            props.content
          )
        )
      )
    }
    .build
}
