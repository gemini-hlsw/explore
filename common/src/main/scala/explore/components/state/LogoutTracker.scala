// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.state

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import explore.events._
import explore.implicits._
import explore.model.UserVault
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.broadcastchannel._
import react.common.ReactFnProps

final case class LogoutTracker(
  setVault:   Option[UserVault] ==> Callback,
  setMessage: NonEmptyString ==> Callback
)(val render: IO[Unit] ==> VdomNode)(implicit val ctx: AppContextIO)
    extends ReactFnProps[LogoutTracker](LogoutTracker.component)

object LogoutTracker {
  type Props = LogoutTracker

  protected implicit val propsReuse: Reusability[Props] =
    Reusability.derive && Reusability.by(_.render)

  protected implicit val stateReuse: Reusability[BroadcastChannel[ExploreEvent]] =
    Reusability.always

  val component = ScalaFnComponent
    .withHooks[Props]
    // Create a nonce
    .useMemo(())(_ => System.currentTimeMillis)
    // Hold the broadcast channel
    .useState(none[BroadcastChannel[ExploreEvent]])
    .useEffectOnMountBy { (props, nonce, state) =>
      val bc = new BroadcastChannel[ExploreEvent]("explore")
      bc.onmessage = (x: ExploreEvent) =>
        // This is coming from the js world, we can't match the type
        (x.event match {
          case ExploreEvent.Logout.event =>
            (props.setVault(none) >> props.setMessage(
              "You logged out in another instance"
            )).to[IO].whenA(x.value.toString =!= nonce.value.toString)
          case _                         => IO.unit
        })

      state
        .setState(bc.some) *> CallbackTo(Callback(bc.close()).attempt)
    }
    .renderWithReuse { (props, nonce, bc) =>
      bc.value.fold[VdomNode](React.Fragment())(bc =>
        props.render(IO(bc.postMessage(ExploreEvent.Logout(nonce.value))).attempt.void)
      )
    }

}
