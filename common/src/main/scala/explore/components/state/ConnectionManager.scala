// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.state

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import explore.AppCtx
import explore.implicits._
import io.chrisdavenport.log4cats.Logger
import io.circe.syntax._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common.ReactProps
import react.semanticui.elements.icon.Icon
import react.semanticui.sizes._

final case class ConnectionManager(ssoToken: NonEmptyString, onConnect: IO[Unit])(
  val render:                                () => VdomNode
) extends ReactProps[ConnectionManager](ConnectionManager.component)

object ConnectionManager {
  type Props = ConnectionManager

  protected case class State(initialized: Boolean = false)

  final class Backend($ : BackendScope[Props, State]) {
    val initialize: IO[Unit] = AppCtx.flatMap(
      _.clients.init(
        $.propsIn[IO].map(props => Map("Authorization" -> s"Bearer ${props.ssoToken.value}".asJson))
      ) >>
        $.setStateIn[IO](State(true)) >>
        $.propsIn[IO].flatMap(_.onConnect)
    )

    def render(props: Props, state: State): VdomNode =
      if (state.initialized)
        props.render()
      else
        Icon(name = "spinner", loading = true, size = Large)
  }

  val component = ScalaComponent
    .builder[Props]
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount(_.backend.initialize.runAsyncCB)
    .componentDidUpdate($ =>
      AppCtx
        .flatMap(implicit ctx =>
          Logger[IO].debug(
            s"[ConnectionManager.componentDidUpdate] Token changed. Terminating connections."
          ) >>
            // We should switch from reestablish() to reinitialize() when ODB supports reinitializing.
            ctx.clients.odb.reestablish()
        )
        .runAsyncCB
        .when($.prevProps.ssoToken =!= $.currentProps.ssoToken)
        .void
    )
    .componentWillUnmountConst( // With code = 1000 we don't attempt reconnection.
      AppCtx
        .flatMap(implicit ctx =>
          Logger[IO].debug(
            s"[ConnectionManager.componentWillUnmount] Terminating connections."
          ) >>
            ctx.clients.close()
        )
        .runAsyncCB
    )
    .build
}
