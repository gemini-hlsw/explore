// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import crystal.react.reuse.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.syntax.all.given
import react.clipboard.CopyToClipboard
import react.common.ReactFnProps
import react.semanticui.modules.modal.Dimmer.Blurring
import react.semanticui.modules.modal.Modal
import react.semanticui.modules.modal.ModalContent

case class About(trigger: Reuse[VdomNode]) extends ReactFnProps(About.component)

object About:
  private type Props = About

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useState(false) // copied
    .render((props, ctx, copied) =>
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
              s"Version: ${ctx.version}",
              CopyToClipboard(
                text = ctx.version.value,
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
