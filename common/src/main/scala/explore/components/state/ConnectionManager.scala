// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.state

import cats.effect.IO
import cats.syntax.all._
import clue.{ TerminateOptions, WebSocketCloseParams }
import crystal.react.implicits._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import explore.AppCtx
import io.circe.syntax._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common.ReactProps

final case class ConnectionManager(ssoToken: NonEmptyString)
    extends ReactProps[ConnectionManager](ConnectionManager.component)

object ConnectionManager {
  type Props = ConnectionManager

  final class Backend($ : BackendScope[Props, Unit]) {
    val connect: IO[Unit] = AppCtx.flatMap(
      _.clients.init(
        $.propsIn[IO].map(props => Map("Authorization" -> s"Bearer ${props.ssoToken.value}".asJson))
      )
    )

    // This is a "phantom" component. Doesn't render anything.
    def render(): VdomNode = React.Fragment()
  }

  val component = ScalaComponent
    .builder[Props]
    .renderBackend[Backend]
    .componentDidMount(_.backend.connect.runAsyncCB)
    .componentDidUpdate($ =>
      AppCtx
        .flatMap(
          // We should change to TerminateOptions.KeepConnection when ODB supports it.
          _.clients.odb.terminate(TerminateOptions.Disconnect())
        ) // This will trigger reconnection strategy and start reconnection attempts.
        .runAsyncCB
        .when($.prevProps.ssoToken =!= $.currentProps.ssoToken)
        .void
    )
    .componentWillUnmountConst( // With code = 1000 we don't attempt reconnection.
      AppCtx
        .flatMap(
          _.clients.odb
            .terminate(TerminateOptions.Disconnect(WebSocketCloseParams(code = 1000)))
        )
        .runAsyncCB
    )
    .build
}
