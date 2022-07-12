// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.syntax.all._
import eu.timepit.refined.types.string._
import explore.HelpContext
import explore.HelpCtx
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.Help
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._

import scala.scalajs.js

final case class HelpIcon(id: Help.Id, clazz: js.UndefOr[Css] = js.undefined)
    extends ReactFnProps[HelpIcon](HelpIcon.component)

object HelpIcon {
  type Props = HelpIcon

  type HelpId = NonEmptyString

  val component = ScalaFnComponent[Props] { p =>
    HelpCtx.usingView { help =>
      val helpMsg = help.zoom(HelpContext.displayedHelp)
      <.span(
        ^.cls :=? p.clazz.map(_.htmlClass),
        ^.onClick ==> { (e: ReactMouseEvent) =>
          e.stopPropagationCB *> e.preventDefaultCB *> helpMsg.set(p.id.some)
        },
        Icons.Info
          .fixedWidth()
          .clazz(ExploreStyles.HelpIcon)
      )
    }
  }
}
