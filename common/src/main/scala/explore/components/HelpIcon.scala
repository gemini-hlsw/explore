// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.syntax.all._
import crystal.react.implicits._
import eu.timepit.refined.types.string._
import explore.HelpContext
import explore.HelpCtx
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.Help
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._

final case class HelpIcon(id: Help.Id)

object HelpIcon {
  type Props = HelpIcon

  type HelpId = NonEmptyFiniteString[20]

  implicit def render(props: HelpIcon): VdomElement = component(props).vdomElement

  val component = ScalaFnComponent[Props] { p =>
    HelpCtx.usingView { help =>
      val helpMsg = help.zoom(HelpContext.displayedHelp)
      <.span(
        ^.onClick ==> { (e: ReactMouseEvent) =>
          e.stopPropagationCB *> e.preventDefaultCB *> helpMsg.set(p.id.some)
        },
        Icons.Info
          .link(true)
          .inverted(true)
          .clazz(ExploreStyles.HelpIcon)
      )
    }
  }
}
