// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.NewType
import lucuma.ui.syntax.all.given
import react.clipboard.CopyToClipboard
import react.common.Css
import react.common.ReactFnProps

case class ExploreCopy(label: String, textToCopy: String, beforeCopyCss: Css, afterCopyCss: Css)
    extends ReactFnProps(ExploreCopy.component)

object ExploreCopy:
  private type Props = ExploreCopy

  private object Copied extends NewType[Boolean]
  private type Copied = Copied.Type

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useState(Copied(false))
    .render((props, copied) =>
      <.div(
        <.span(
          props.beforeCopyCss,
          props.afterCopyCss.unless(copied.value.value)
        )(
          props.label,
          CopyToClipboard(
            text = props.textToCopy,
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
