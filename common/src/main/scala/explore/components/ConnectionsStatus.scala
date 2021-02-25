// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import clue.PersistentClientStatus
import clue.PersistentClientStatus._
import crystal.Error
import crystal.Pending
import crystal.Pot
import crystal.Ready
import explore.AppCtx
import explore.components.ui.ExploreStyles
import explore.components.ui.ExploreStyles._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common.ReactProps
import react.semanticui.elements.icon._
import react.semanticui.modules.popup._
import react.semanticui.views.item.Item

final case class ConnectionsStatus()
    extends ReactProps[ConnectionsStatus](ConnectionsStatus.component)

object ConnectionsStatus {
  type Props = ConnectionsStatus

  private def renderStatus(name: String)(status: Pot[PersistentClientStatus]): VdomElement = {
    val (message, (clazz, show)) = status match {
      case Error(t)     => (t.getMessage, (ConnectionError, true))
      case Pending(_)   => ("Mounting...", (ConnectionWarning, true))
      case Ready(value) =>
        (value.toString,
         value match {
           case Connecting                             => (ConnectionWarning, true)
           case Connected | Initializing | Initialized => (ConnectionOK, false)
           case Disconnected                           => (ConnectionError, true)
         }
        )
    }

    if (show) {
      Item(clazz = ExploreStyles.ConnectionIcon)(
        Popup(
          header = s"$name Connection Status",
          content = message,
          position = PopupPosition.BottomRight,
          trigger = Icon(name = "circle", fitted = true, clazz = clazz)
        )
      ).vdomElement
    } else <.span()
  }

  val component = ScalaComponent
    .builder[Props]
    .render(_ =>
      AppCtx.withCtx { ctx =>
        ctx.clients.ODBConnectionStatus(renderStatus("ODB"))
      }
    )
    .build

}
