// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.fa.IconSize
import lucuma.react.primereact.Tooltip
import lucuma.react.primereact.tooltip.*

final case class CustomizedGroupAddon(
  original: String,
  toRevert: Callback
) extends ReactFnProps(CustomizedGroupAddon)

object CustomizedGroupAddon
    extends ReactFnComponent[CustomizedGroupAddon](props =>
      <.span(
        ^.cls := "fa-layers fa-fw",
        Icons.ExclamationDiamond
          .withClass(ExploreStyles.WarningIcon)
          .withSize(IconSize.X1),
        ^.onClick --> props.toRevert
      ).withTooltip(
        content = <.div("Customized!", <.br, s"Click to revert to '${props.original}'"),
        position = Tooltip.Position.Left // putting it on the left should always work.
      )
    )
