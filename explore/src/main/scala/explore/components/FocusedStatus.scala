// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.syntax.all.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Focused
import explore.model.enums.AppTab
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.react.common.ReactFnProps

case class FocusedStatus(tab: AppTab, programId: Program.Id, focused: Focused)
    extends ReactFnProps(FocusedStatus.component)

/**
 * Development utility to show the current focused state
 */
object FocusedStatus:
  private type Props = FocusedStatus

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .render: (props, ctx) =>
        <.div(
          ExploreStyles.FocusedInfo,
          <.div(
            s"Target: ${props.focused.target.foldMap(_.show)}, obs: ${props.focused.obsSet.foldMap(_.show)}"
          ),
          <.div(ctx.pageUrl((props.tab, props.programId, props.focused).some))
        )
