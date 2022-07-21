// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.state

import cats.effect.IO
import cats.syntax.all._
import crystal.react.hooks._
import crystal.react.implicits._
import eu.timepit.refined.types.string.NonEmptyString
import explore.implicits._
import io.circe.Json
import io.circe.syntax._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.typelevel.log4cats.Logger
import react.common._
import react.semanticui.elements.loader.Loader

final case class ConnectionManager(ssoToken: NonEmptyString, onConnect: IO[Unit])(
  val render:                                VdomNode
)(implicit val ctx:                          AppContextIO)
    extends ReactFnProps[ConnectionManager](ConnectionManager.component) {
  val payload: Map[String, Json] =
    Map("Authorization" -> s"Bearer ${props.ssoToken.value}".asJson)
}

object ConnectionManager {
  protected type Props = ConnectionManager

  protected val component = ScalaFnComponent
    .withHooks[Props]
    .useState(false) // initialized as state, which forces rerender on set
    .useRef(false)   // initialized as ref, which can be read asynchronously by cleanup
    .useEffectWithDepsBy((props, _, _) => props.ssoToken.value) {
      (props, initializedState, _) => _ =>
        implicit val ctx = props.ctx

        (Logger[IO].debug(s"[ConnectionManager] Token changed. Refreshing connections.") >>
          ctx.clients.odb.initialize(props.payload)).whenA(initializedState.value)
    }
    .useAsyncEffectOnMountBy { (props, initializedState, initializedRef) =>
      implicit val ctx = props.ctx

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
    .render((props, initializedState, _) =>
      if (initializedState.value)
        props.render
      else
        Loader(active = true)
    )
}
