// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.fa.IconSize
import react.semanticui.modules.popup.Popup

package object itc {
  def requiredForITC: TagMod =
    Popup(
      trigger = <.span(
        ^.cls := "fa-layers fa-fw",
        Icons.StarExclamation
          .clazz(ExploreStyles.WarningIcon)
          .size(IconSize.X1),
        <.span(^.cls := "fa-layers-text fa-inverse", "ITC")
      )
    )("Required for ITC")

}
