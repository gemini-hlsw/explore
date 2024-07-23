// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import clue.PersistentClientStatus
import clue.PersistentClientStatus.*
import crystal.Pot
import crystal.react.hooks.*
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import lucuma.react.floatingui.Placement
import lucuma.react.floatingui.syntax.*
import lucuma.ui.LucumaStyles
import lucuma.ui.syntax.all.given

case class ConnectionsStatus() extends ReactFnProps(ConnectionsStatus.component)

object ConnectionsStatus:
  private type Props = ConnectionsStatus

  private def renderStatus(status: Pot[PersistentClientStatus]): VdomNode =
    val (message, (clazz, show)) = status match
      case Pot.Pending      => ("Mounting...", (LucumaStyles.IndicatorWarning, true))
      case Pot.Error(t)     => (t.getMessage, (LucumaStyles.IndicatorFail, true))
      case Pot.Ready(value) =>
        (value.toString,
         value match
           case Connected    => (LucumaStyles.IndicatorOK, false)
           case Connecting   => (LucumaStyles.IndicatorWarning, true)
           case Disconnected => (LucumaStyles.IndicatorFail, true)
        )

    if (show)
      <.div(
        ExploreStyles.ConnectionIcon,
        <.span(Icons.CircleSmall.withClass(clazz)).withTooltip(
          tooltip = message,
          placement = Placement.Bottom
        )
      )
    else EmptyVdom

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStreamOnMountBy((_, ctx) => ctx.clients.odb.statusStream)
      .render((_, _, status) => renderStatus(status.toPot))
