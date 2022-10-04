// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.syntax.all.*
import eu.timepit.refined.types.string.*
import explore.HelpContext
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.Help
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.common.Css
import react.common.ReactFnProps

import scala.scalajs.js

case class HelpIcon(id: Help.Id, clazz: js.UndefOr[Css] = js.undefined)
    extends ReactFnProps[HelpIcon](HelpIcon.component)

object HelpIcon:
  type Props = HelpIcon

  type HelpId = NonEmptyString

  val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(HelpContext.ctx)
    .render { (props, help) =>
      <.span(
        ^.cls :=? props.clazz.map(_.htmlClass),
        ^.onClick ==> { (e: ReactMouseEvent) =>
          e.stopPropagationCB *> e.preventDefaultCB *> help.displayedHelp.set(props.id.some)
        },
        Icons.Info
          .fixedWidth()
          .clazz(ExploreStyles.HelpIcon)
      )
    }
