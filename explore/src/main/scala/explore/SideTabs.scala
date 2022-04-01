// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.syntax.all._
import explore.components.ui.ExploreStyles
import explore.model.RoutingInfo
import explore.model.enum.AppTab
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
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
  routingInfo: RoutingInfo
) extends ReactFnProps[SideTabs](SideTabs.component)

object SideTabs {
  type Props = SideTabs

  implicit val propsReuse: Reusability[Props] = Reusability.derive

  protected val component =
    ScalaFnComponent
      .withReuse[Props] { p =>
        AppCtx.using { implicit ctx =>
          val focus = p.routingInfo.appTab

          def onClickE[A](tab: AppTab) =
            linkOverride[A](ctx.setPage(tab, p.routingInfo.focusedObs, p.routingInfo.focusedTarget))

          def tabButton(tab: AppTab): Button =
            Button(
              as = <.a,
              active = tab === focus,
              clazz = ExploreStyles.SideButton,
              onClickE = onClickE[ButtonProps](tab)
            )(^.href := ctx.pageUrl(tab, p.routingInfo.focusedObs, p.routingInfo.focusedTarget),
              tab.title
            )

          def tab(tab: AppTab): Label =
            Label(
              as = <.a,
              active = tab === focus,
              clazz = ExploreStyles.TabSelector,
              size = Tiny,
              onClickE = onClickE[LabelProps](tab)
            )(^.href := ctx.pageUrl(tab, none, none), tab.title)

          def makeButtonSection(tabs: List[AppTab]): TagMod = tabs match {
            case justOne :: Nil => VerticalSection()(tabButton(justOne))
            case _              =>
              VerticalSection()(
                ButtonGroup(tabs.reverse.map(tabButton).toTagMod)
              )
          }

          val verticalButtonsSections: List[TagMod] =
            AppTab.all.toList
              .groupBy(_.buttonGroup)
              .toList
              .sortBy(_._1)
              .map(tup => makeButtonSection(tup._2))

          val horizontalButtonsSections: List[TagMod] =
            AppTab.all.toList
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
}
