// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.state

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.hooks.*
import crystal.react.implicits.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.AppContext
import io.circe.Json
import io.circe.syntax.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.ui.syntax.all.given
import org.typelevel.log4cats.Logger
import react.common.ReactFnPropsWithChildren
import react.semanticui.elements.loader.Loader

case class ConnectionManager(ssoToken: NonEmptyString, onConnect: IO[Unit])
    extends ReactFnPropsWithChildren(ConnectionManager.component):
  val payload: Map[String, Json] = Map("Authorization" -> s"Bearer ${ssoToken.value}".asJson)

object ConnectionManager {
  private type Props = ConnectionManager

  private val component = ScalaFnComponent
    .withHooks[Props]
    .withPropsChildren
    .useContext(AppContext.ctx)
    .useState(false) // initialized as state, which forces rerender on set
    .useRef(false)   // initialized as ref, which can be read asynchronously by cleanup
    .useEffectWithDepsBy((props, _, _, _, _) => props.ssoToken.value) {
      (props, _, ctx, initializedState, _) => _ =>
        import ctx.given

        (Logger[IO].debug(s"[ConnectionManager] Token changed. Refreshing connections.") >>
          ctx.clients.odb.initialize(props.payload)).whenA(initializedState.value)
    }
    .useAsyncEffectOnMountBy { (props, _, ctx, initializedState, initializedRef) =>
      import ctx.given

      val initialize: IO[Unit] =
        ctx.clients.init(props.payload) >>
          initializedRef.setAsync(true) >>
          initializedState.setStateAsync(true) >>
          props.onConnect

      val cleanup: IO[Unit] =
        initializedRef.getAsync >>= (initialized =>
          (Logger[IO].debug(s"[ConnectionManager] Terminating connections.") >>
            ctx.clients.close()).whenA(initialized)
        )

      initialize.as(cleanup)
    }
    .render((props, children, _, initializedState, _) =>
      if (initializedState.value)
        children
      else
        Loader(active = true)
    )
}
