// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.graphql

import cats.data.NonEmptyList
import cats.effect.Async
import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.std.Queue
import cats.syntax.all._
import clue.GraphQLSubscription
import clue.WebSocketClient
import crystal.Pot
import crystal.ViewF
import crystal.react._
import explore._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.typelevel.log4cats.Logger
import react.common._
import react.semanticui.collections.message.Message
import react.semanticui.elements.icon.Icon
import react.semanticui.sizes._

import scala.concurrent.duration._
import scala.language.postfixOps

final case class LiveQueryRenderMod[S, D, A](
  query:               IO[D],
  extract:             D => A,
  changeSubscriptions: NonEmptyList[IO[GraphQLSubscription[IO, _]]]
)(
  val valueRender:     View[A] ~=> VdomNode,
  val pendingRender:   Long ~=> VdomNode =
    Reusable.always(_ => Icon(name = "spinner", loading = true, size = Large)),
  val errorRender:     Throwable ~=> VdomNode =
    Reusable.always(t => Message(error = true)(t.getMessage)),
  val onNewData:       IO[Unit] = IO.unit
)(implicit
  val F:               Async[IO],
  val dispatcher:      Dispatcher[IO],
  val logger:          Logger[IO],
  val reuse:           Reusability[A],
  val client:          WebSocketClient[IO, S]
) extends ReactProps(LiveQueryRenderMod.component)
    with LiveQueryRenderMod.Props[IO, S, D, A]

object LiveQueryRenderMod {
  trait Props[F[_], S, D, A] extends Render.LiveQuery.Props[F, ViewF[F, *], S, D, A]

  final case class State[F[_], S, D, A](
    queue:                   Queue[F, A],
    subscriptions:           NonEmptyList[GraphQLSubscription[F, _]],
    cancelConnectionTracker: F[Unit],
    renderer:                StreamRendererMod.Component[F, A]
  ) extends Render.LiveQuery.State[F, ViewF[F, *], S, D, A]

  // Reusability should be controlled by enclosing components and reuse parameter. We allow rerender every time it's requested.
  implicit def propsReuse[F[_], S, D, A]: Reusability[Props[F, S, D, A]]                 = Reusability.never
  implicit def stateReuse[F[_], S, D, A]: Reusability[State[F, S, D, A]]                 = Reusability.never
  implicit protected def renderReuse[F[_], A]: Reusability[Pot[ViewF[F, A]] => VdomNode] =
    Reusability.never

  protected def componentBuilder[F[_], S, D, A] =
    ScalaComponent
      .builder[Props[F, S, D, A]]
      .initialState[Option[State[F, S, D, A]]](none)
      .render(Render.renderFn[F, ViewF[F, *], D, A](_))
      .componentDidMount(
        Render.LiveQuery
          .didMountFn[F, ViewF[F, *], S, D, A][Props[F, S, D, A], State[F, S, D, A]](
            "LiveQueryRenderMod",
            (stream, props) => {
              implicit val F          = props.F
              implicit val dispatcher = props.dispatcher
              implicit val logger     = props.logger
              implicit val reuse      = props.reuse

              StreamRendererMod.build(stream, holdAfterMod = (2 seconds).some)
            },
            State.apply
          )
      )
      .componentWillUnmount(Render.LiveQuery.willUnmountFn[F, ViewF[F, *], S, D, A](_))
      .configure(Reusability.shouldComponentUpdate)
      .build

  val component = componentBuilder[IO, Any, Any, Any]
}
