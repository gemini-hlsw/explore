// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import explore.components.ui.ExploreStyles
import explore.model.enum.AppTab
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.data.EnumZipper
import lucuma.ui.reusability._
import lucuma.ui.utils._
import react.common._
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.Button.ButtonProps
import react.semanticui.elements.button.ButtonGroup
import react.semanticui.elements.divider.Divider
import react.semanticui.elements.label.Label
import react.semanticui.elements.label.Label.LabelProps
import react.semanticui.sizes._

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
        AppCtx.withCtx { ctx =>
          val tabsL = p.tabs.get.toNel
          val focus = p.tabs.get.focus

          def onClickE[A](tab: AppTab) =
            linkOverride[IO, A](p.tabs.mod(z => z.findFocus(_ === tab).getOrElse(z)))

          def tabButton(tab:          AppTab): Button       =
            Button(
              as = <.a,
              active = tab === focus,
              clazz = ExploreStyles.SideButton,
              onClickE = onClickE[ButtonProps](tab)
            )(^.href := ctx.pageUrl(tab, none), tab.title)

          def tab(tab:                AppTab): Label        =
            Label(
              as = <.a,
              active = tab === focus,
              clazz = ExploreStyles.TabSelector,
              size = Tiny,
              onClickE = onClickE[LabelProps](tab)
            )(^.href := ctx.pageUrl(tab, none), tab.title)

          def makeButtonSection(tabs: List[AppTab]): TagMod = tabs match {
            case justOne :: Nil => VerticalSection()(tabButton(justOne))
            case _              =>
              VerticalSection()(
                ButtonGroup(tabs.reverse.map(tabButton).toTagMod)
              )
          }

          val verticalButtonsSections: List[TagMod] =
            tabsL.toList
              .groupBy(_.buttonGroup)
              .toList
              .sortBy(_._1)
              .map(tup => makeButtonSection(tup._2))

          val horizontalButtonsSections: List[TagMod] =
            tabsL.toList.toList
              .map(tup => tab(tup))

          React.Fragment(
            <.div(
              ExploreStyles.SideTabsVertical,
              verticalButtonsSections.mkTagMod(
                Divider(hidden = true, clazz = ExploreStyles.SideTabsDivider)
              )
            ),
            <.div(
              ExploreStyles.SideTabsHorizontal,
              horizontalButtonsSections.toTagMod
            )
          )
        }
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
