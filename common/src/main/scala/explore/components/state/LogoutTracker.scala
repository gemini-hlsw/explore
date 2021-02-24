// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.state

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.UserVault
import explore.utils.ExploreEvent
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.broadcastchannel._
import monocle.macros.Lenses
import react.common.ReactProps

final case class LogoutTracker(
  setVault:   Option[UserVault] => IO[Unit],
  setMessage: NonEmptyString => IO[Unit]
)(val render: IO[Unit] => VdomNode)
    extends ReactProps[LogoutTracker](LogoutTracker.component)

object LogoutTracker {
  type Props = LogoutTracker

  @Lenses
  case class State(bc: Option[BroadcastChannel[ExploreEvent]])

  private val component =
    ScalaComponent
      .builder[LogoutTracker]
      .initialState(State(none))
      .render { $ =>
        React.Fragment(
          $.state.bc.fold[VdomNode](React.Fragment())(bc =>
            $.props.render(IO(bc.postMessage(ExploreEvent.Logout)).attempt.void)
          )
        )
      }
      .componentDidMount { $ =>
        IO {
          val bc = new BroadcastChannel[ExploreEvent]("explore")
          bc.onmessage = (x: ExploreEvent) =>
            // This is coming from the js world, we can't match the type
            (x.event match {
              case ExploreEvent.Logout.event =>
                $.props.setVault(none) >> $.props.setMessage("You logged out in another instance")
              case _                         => IO.unit
            })
          bc
        }.flatMap(bc => $.modStateIn[IO](State.bc.set(bc.some))).runAsyncCB
      }
      .componentWillUnmount($ =>
        // Setting vault to none is defensive. This component should actually unmount when vault is none.
        $.state.bc.map(bc => IO(bc.close()).attempt.void).orEmpty.runAsyncCB
      )
      .build

}
