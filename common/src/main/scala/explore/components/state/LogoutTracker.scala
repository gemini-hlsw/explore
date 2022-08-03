// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.state

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import explore.events._
import explore.implicits._
import explore.model.UserVault
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.broadcastchannel._
import lucuma.refined._
import react.common.ReactFnProps

final case class LogoutTracker(
  setVault:   Option[UserVault] => Callback,
  setMessage: NonEmptyString => Callback
)(val render: IO[Unit] => VdomNode)(implicit val ctx: AppContextIO)
    extends ReactFnProps[LogoutTracker](LogoutTracker.component)

object LogoutTracker {
  type Props = LogoutTracker

  val component = ScalaFnComponent
    .withHooks[Props]
    // Create a nonce
    .useMemo(())(_ => System.currentTimeMillis)
    // Hold the broadcast channel
    .useState(none[BroadcastChannel[ExploreEvent]])
    .useEffectOnMountBy { (props, nonce, state) =>
      val bc = new BroadcastChannel[ExploreEvent]("explore")
      bc.onmessage = (
        (x: ExploreEvent) =>
          // This is coming from the js world, we can't match the type
          x.event match {
            case ExploreEvent.LogoutEvent.event =>
              (props.setVault(none) >> props.setMessage(
                "You logged out in another instance".refined
              )).to[IO].whenA(x.value.toString =!= nonce.value.toString)
            case _                              => IO.unit
          }
      ): (ExploreEvent => IO[Unit]) // Scala 3 infers the return type as Any if we don't ascribe

      state
        .setState(bc.some) *> CallbackTo(Callback(bc.close()).attempt)
    }
    .render { (props, nonce, bc) =>
      bc.value.fold[VdomNode](React.Fragment())(bc =>
        props.render(IO(bc.postMessage(ExploreEvent.LogoutEvent(nonce.value))).attempt.void)
      )
    }

}
