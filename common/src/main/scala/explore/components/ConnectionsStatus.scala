// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import clue.StreamingClientStatus._
import crystal.Error
import crystal.Pending
import crystal.Ready
import explore.AppCtx
import explore.components.ui.ExploreStyles._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common.ReactProps
import react.semanticui.elements.icon._
import react.semanticui.modules.popup._
import crystal.Pot
import clue.StreamingClientStatus

final case class ConnectionsStatus()
    extends ReactProps[ConnectionsStatus](ConnectionsStatus.component)

object ConnectionsStatus {
  type Props = ConnectionsStatus

  private def renderStatus(name: String)(status: Pot[StreamingClientStatus]): VdomNode = {
    val (message, clazz) = status match {
      case Error(t)     => (t.getMessage, ConnectionError)
      case Pending(_)   => ("Mounting...", ConnectionWarning)
      case Ready(value) =>
        value match {
          case Connecting => ("Connecting...", ConnectionWarning)
          case Open       => ("Connected", ConnectionOK)
          case Closing    => ("Closing...", ConnectionWarning)
          case Closed     => ("Closed", ConnectionError)
        }
    }

    println(s"$name Connection Status: $message")

    Popup(
      header = s"$name Connection Status",
      content = message,
      position = PopupPosition.BottomRight,
      trigger = Icon(name = "circle", clazz = clazz)
    )
  }

  val component = ScalaComponent
    .builder[Props]
    .render(_ =>
      AppCtx.withCtx { ctx =>
        <.span(
          ctx.clients.ODBConnectionStatus(renderStatus("ODB")),
          ctx.clients.ObservationDBConnectionStatus(renderStatus("Lucuma ODB"))
        )
      }
    )
    .build

}
