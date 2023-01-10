// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import crystal.react.View
import crystal.react.reuse.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.NewType
import lucuma.ui.syntax.all.given
import react.clipboard.CopyToClipboard
import react.common.ReactFnProps
import react.primereact.Dialog

case class About(isOpen: View[Boolean]) extends ReactFnProps(About.component)

object About:
  private type Props = About

  private object Copied extends NewType[Boolean]
  private type Copied = Copied.Type

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useState(Copied(false))
    .render((props, ctx, copied) =>
      Dialog(
        visible = props.isOpen.get,
        onHide = props.isOpen.set(false),
        dismissableMask = true,
        clazz = ExploreStyles.Dialog.Small,
        resizable = false,
        header = Logo()
      )(
        <.div(
          <.span(
            ExploreStyles.Version,
            ExploreStyles.VersionUncopied.when(!copied.value.value)
          )(
            s"Version: ${ctx.version}",
            CopyToClipboard(
              text = ctx.version.value,
              onCopy = (_, copiedCallback) =>
                copied.setState(Copied(copiedCallback)) *>
                  copied.setState(Copied(false)).delayMs(1500).toCallback
            )(
              <.span(
                Icons.Clipboard.unless(copied.value.value),
                Icons.ClipboardCheck.when(copied.value.value)
              )
            )
          )
        )
      )
    )
