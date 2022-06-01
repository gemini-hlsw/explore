// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import crystal.react.reuse._
import explore.Icons
import explore._
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.clipboard.CopyToClipboard
import react.common.ReactFnProps
import react.semanticui.modules.modal.Dimmer.Blurring
import react.semanticui.modules.modal.Modal
import react.semanticui.modules.modal.ModalContent
import react.semanticui.shorthand._

case class About(trigger: Reuse[VdomNode])(implicit val ctx: AppContextIO)
    extends ReactFnProps[About](About.component)

object About {
  type Props = About

  val component = ScalaFnComponent
    .withHooks[Props]
    .useState(false) // copied
    .render((props, copied) =>
      Modal(
        dimmer = Blurring,
        trigger = props.trigger: VdomNode,
        closeIcon = Icons.Close.clazz(ExploreStyles.ModalCloseButton),
        content = ModalContent(
          <.div(
            Logo(),
            <.span(
              ExploreStyles.Version,
              ExploreStyles.VersionUncopied.when(!copied.value)
            )(
              s"Version: ${props.ctx.version}",
              CopyToClipboard(
                text = props.ctx.version.value,
                onCopy = (_, copiedCallback) =>
                  copied.setState(copiedCallback) *>
                    copied.setState(false).delayMs(1500).toCallback
              )(
                <.span(
                  Icons.Clipboard.unless(copied.value),
                  Icons.ClipboardCheck.when(copied.value)
                )
              )
            )
          )
        )
      )
    )
}
