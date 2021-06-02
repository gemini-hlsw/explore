// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.state

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import explore.implicits._
import explore.model.Clients
import io.circe.Json
import io.circe.syntax._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.reuse._
import org.typelevel.log4cats.Logger
import react.common.ReactProps
import react.semanticui.elements.loader.Loader

final case class ConnectionManager(ssoToken: NonEmptyString, onConnect: IO[Unit])(
  val render:                                Reuse[VdomNode]
)(implicit val ctx:                          AppContextIO)
    extends ReactProps[ConnectionManager](ConnectionManager.component)

object ConnectionManager {
  type Props = ConnectionManager

  protected case class State(initialized: Boolean = false)

  final class Backend($ : BackendScope[Props, State]) {
    val payload: IO[Map[String, Json]] =
      $.propsIn[IO].map(props => Map("Authorization" -> s"Bearer ${props.ssoToken.value}".asJson))

    def initialize(clients: Clients[IO]): IO[Unit] =
      payload >>= (p => clients.init(p))

    def refresh(clients: Clients[IO]): IO[Unit] =
      payload >>= (p => clients.odb.initialize(p))

    def onMount(clients: Clients[IO]): IO[Unit] =
      initialize(clients) >>
        $.setStateIn[IO](State(true)) >>
        $.propsIn[IO].flatMap(_.onConnect)

    def render(props: Props, state: State): VdomNode =
      if (state.initialized)
        props.render
      else
        Loader(active = true)
  }

  val component = ScalaComponent
    .builder[Props]
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount { $ =>
      implicit val ctx = $.props.ctx
      $.backend.onMount(ctx.clients).runAsyncCB
    }
    .componentDidUpdate { $ =>
      implicit val ctx = $.currentProps.ctx
      (Logger[IO].debug(s"[ConnectionManager] Token changed. Refreshing connections.") >>
        $.backend.refresh(ctx.clients))
        .whenA($.prevProps.ssoToken =!= $.currentProps.ssoToken)
        .runAsyncCB
    }
    .componentWillUnmount { $ =>
      implicit val ctx = $.props.ctx
      if ($.state.initialized)
        (Logger[IO].debug(s"[ConnectionManager] Terminating connections.") >>
          ctx.clients.close()).runAsyncCB
      else Callback.empty
    }
    .build
}
