// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.syntax.all._
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

  // This is a protected method on cats.Foldable. If we add
  // fs2 to the project, we can switch to intersperse on fs2.Stream
  def intersperseList[A](xs: List[A], x: A): List[A] = {
    val bld = List.newBuilder[A]
    val it  = xs.iterator
    if (it.hasNext) {
      bld += it.next()
      while (it.hasNext) {
        bld += x
        bld += it.next()
      }
    }
    bld.result()
  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .stateless
      .render_P { p =>
        val tabsL = p.tabs.get.toNel
        val focus = p.tabs.get.focus

        // group the tab buttons. Setting the value to 0 will
        // cause the button to NOT be displayed.
        def tabGrouping(tab: AppTab): Integer = tab match {
          case AppTab.Overview       => 1
          case AppTab.Observations   => 2
          case AppTab.Targets        => 2
          case AppTab.Configurations => 2
          case AppTab.Constraints    => 2
        }

        def tabButton(tab: AppTab): Button =
          Button(active = tab === focus,
                 onClick = p.tabs.mod(z => z.findFocus(_ === tab).getOrElse(z)).runInCB
          )(tab.title)

        def makeButtonSection(tabs: List[AppTab]): TagMod = tabs match {
          case justOne :: Nil => VerticalSection()(tabButton(justOne))
          case _              => VerticalSection()(ButtonGroup(tabs.reverse.map(tabButton).toTagMod))
        }

        val buttonSections: List[TagMod] =
          tabsL.toList
            .map(t => (tabGrouping(t), t))
            .groupMap(_._1)(_._2)
            .toList
            .mapFilter { tup =>
              if (tup._1 > 0) Some(makeButtonSection(tup._2))
              else None
            }

        val taglist: List[TagMod] = intersperseList(buttonSections, Divider(hidden = true))

        <.div(
          GPPStyles.SideTabsBody,
          taglist.toTagMod
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
