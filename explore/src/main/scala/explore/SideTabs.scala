// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.implicits._
import crystal.react.implicits._
import explore.components.ui.GPPStyles
import explore.model.enum.AppTab
import gem.data.EnumZipper
import lucuma.ui.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.ButtonGroup
import react.semanticui.elements.divider.Divider

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

        def tabButton(style: Css)(tab: AppTab): Button =
          Button(active = tab === focus,
                 clazz = style,
                 onClick = p.tabs.mod(z => z.findFocus(_ === tab).getOrElse(z)).runInCB
          )(tab.title)

        <.div(
          GPPStyles.SideTabsBody,
          VerticalSection()(
            tabButton(Css.Empty)(tabsL.head)
          ),
          Divider(hidden = true),
          VerticalSection()(
            ButtonGroup(
              // Due to the css rotations these need to be in reversed order
              tabsL.tail.reverse
                .map(tabButton(GPPStyles.SideButton))
                .toTagMod
            )
          )
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
