// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import scala.scalajs.js.JSConverters._

import cats.implicits._
import crystal.react.implicits._
import crystal.data.react.implicits._
import explore.components.ui.GPPStyles
import explore.model.Page
import explore.model.enum.AppTab
import explore.model.reusability._
import gem.data.EnumZipper
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.RouterCtl
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.common.implicits._
import react.semanticui.SemanticWidth
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.ButtonGroup
import react.semanticui.elements.divider.Divider
import react.semanticui.sizes._
import react.semanticui.widths._

final case class SideTabs(
  tabs: View[EnumZipper[AppTab]]
) extends ReactProps[SideTabs](SideTabs.component)

object SideTabs {
  type Props = SideTabs

  implicit val propsReuse: Reusability[Props] = Reusability.derive

  protected val component =
    ScalaComponent
      .builder[Props]
      .stateless
      .render_P { p =>
        val tabsL = p.tabs.get.toNel
        val focus = p.tabs.get.focus

        def tabButton(tab: AppTab): Button =
          Button(active = tab === focus,
                 onClick = p.tabs.mod(z => z.findFocus(_ === tab).getOrElse(z)).runInCB
          )(tab.title)

        <.div(
          GPPStyles.SideTabsBody,
          VerticalSection()(
            tabButton(tabsL.head)
          ),
          Divider(hidden = true),
          VerticalSection()(
            ButtonGroup(widths = widthOf(tabsL.length))(
              // Due to the css rotations these need to be in reversed order
              tabsL.tail.reverse
                .map(tabButton)
                .toTagMod
            )
          )
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
