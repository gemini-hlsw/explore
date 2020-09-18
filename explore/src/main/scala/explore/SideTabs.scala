// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.implicits._
import crystal.react.implicits._
import explore.components.ui.GPPStyles
import explore.model.enum.AppTab
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.data.EnumZipper
import lucuma.ui.reusability._
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

        def tabButton(tab: AppTab): Button =
          Button(active = tab === focus,
                 onClick = p.tabs.mod(z => z.findFocus(_ === tab).getOrElse(z)).runInCB
          )(tab.title)

        def makeButtonSection(tabs: List[AppTab]): TagMod = tabs match {
          case justOne :: Nil => VerticalSection()(tabButton(justOne))
          case _              =>
            VerticalSection()(
              ButtonGroup(tabs.sortBy(_.groupOrder.value).reverse.map(tabButton).toTagMod)
            )
        }

        val buttonSections: List[TagMod] =
          tabsL.toList
            .groupBy(_.buttonGroup)
            .toList
            .sortBy(_._1)
            .map(tup => makeButtonSection(tup._2))

        <.div(
          GPPStyles.SideTabsBody,
          buttonSections.mkTagMod(Divider(hidden = true))
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
