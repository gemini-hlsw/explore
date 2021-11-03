// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.state

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import explore.implicits._
import explore.model.UserVault
import explore.utils.ExploreEvent
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.broadcastchannel._
import monocle.Focus
import react.common.ReactProps
// import japgolly.scalajs.react.callback.CallbackCats._

final case class LogoutTracker(
  setVault:   Option[UserVault] ==> Callback,
  setMessage: NonEmptyString ==> Callback
)(val render: IO[Unit] ==> VdomNode)(implicit val ctx: AppContextIO)
    extends ReactProps[LogoutTracker](LogoutTracker.component)

object LogoutTracker {
  type Props = LogoutTracker

  case class State(bc: Option[BroadcastChannel[ExploreEvent]])

  object State {
    val bc = Focus[State](_.bc)
  }

  protected implicit val propsReuse: Reusability[Props] =
    Reusability.derive && Reusability.by(_.render)
  protected implicit val stateReuse: Reusability[State] = Reusability.never

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
        CallbackTo {
          val bc = new BroadcastChannel[ExploreEvent]("explore")
          bc.onmessage = (x: ExploreEvent) =>
            // This is coming from the js world, we can't match the type
            (x.event match {
              case ExploreEvent.Logout.event =>
                ($.props.setVault(none) >> $.props.setMessage(
                  "You logged out in another instance"
                )).to[IO]
              case _                         => IO.unit
            })
          bc
        }.flatMap(bc => $.modState(State.bc.replace(bc.some)))
      }
      .componentWillUnmount { $ =>
        implicit val ctx = $.props.ctx
        $.state.bc.map(bc => IO(bc.close()).attempt.void).orEmpty.runAsyncAndForget
      }
      .configure(Reusability.shouldComponentUpdate)
      .build

}
