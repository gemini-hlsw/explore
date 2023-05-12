// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.hooks.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.RoutingInfo
import explore.model.enums.AppTab
import explore.syntax.ui.*
import explore.syntax.ui.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.DefaultEffects.{Sync => DefaultS}
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*
import react.common.Css
import react.common.ReactFnProps
import react.primereact.Button
import react.primereact.Divider
import react.primereact.SelectButton

case class SideTabs(routingInfo: RoutingInfo) extends ReactFnProps(SideTabs.component)

object SideTabs:
  private type Props = SideTabs

  private given Display[AppTab] = _.title

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView(Enumerated[AppTab].all.head)
      .useEffectWithDepsBy((p, _, _) => p.routingInfo.appTab)((_, _, selected) =>
        focus => selected.set(focus)
      )
      .render { (p, ctx, tabs) =>
        val focus = p.routingInfo.appTab
        val ri    = p.routingInfo

        val tabView = tabs.withOnMod(tab => ctx.pushPage(tab, ri.programId, ri.focused))

        val groupCssClasses =
          Map(1 -> ExploreStyles.SideTabGroup, 2 -> ExploreStyles.SideTabGroup, 3 -> Css.Empty)

        React.Fragment(
          <.div(
            ExploreStyles.SideTabsVertical,
            SelectButtonEnumView(
              "side-tabs".refined,
              tabView,
              buttonClass = ExploreStyles.SideButton,
              itemTemplate = tab =>
                <.div(
                  ExploreStyles.RotationWrapperOuter |+|
                    groupCssClasses.getOrElse(tab.value.buttonGroup, Css.Empty),
                  <.div(
                    ExploreStyles.RotationWrapperInner,
                    <.a(
                      ^.onClick ==> ((e: ReactEvent) => e.preventDefaultCB),
                      ^.href := ctx.pageUrl(tab.value, ri.programId, ri.focused),
                      ExploreStyles.VerticalButton,
                      tab.value.title
                    )
                  )
                )
            )
          ),
          <.div(
            SelectButtonEnumView(
              "side-tabs".refined,
              tabView,
              groupClass = ExploreStyles.SideTabsHorizontal,
              buttonClass = ExploreStyles.TabSelector,
              itemTemplate = tab =>
                <.a(
                  ^.onClick ==> ((e: ReactEvent) => e.preventDefaultCB),
                  ^.href := ctx.pageUrl(tab.value, ri.programId, ri.focused),
                  tab.value.title
                )
            )
          )
        )
      }
