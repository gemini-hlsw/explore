// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.graphql

import cats.Id
import cats.data.NonEmptyList
import cats.effect.CancelToken
import cats.effect.ConcurrentEffect
import cats.effect.IO
import cats.syntax.all._
import clue.GraphQLSubscription
import clue.WebSocketClient
import crystal.react._
import explore.utils._
import fs2.concurrent.Queue
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.typelevel.log4cats.Logger
import react.common._
import react.semanticui.collections.message.Message
import react.semanticui.elements.icon.Icon
import react.semanticui.sizes._

final case class LiveQueryRender[S, D, A](
  query:               IO[D],
  extract:             D => A,
  changeSubscriptions: NonEmptyList[IO[GraphQLSubscription[IO, _]]]
)(
  val valueRender:     A ==> VdomNode,
  val pendingRender:   Long ==> VdomNode =
    Reuse.always(_ => Icon(name = "spinner", loading = true, size = Large)),
  val errorRender:     Throwable ==> VdomNode = Reuse.always(t => Message(error = true)(t.getMessage)),
  val onNewData:       IO[Unit] = IO.unit
)(implicit
  val F:               ConcurrentEffect[IO],
  val logger:          Logger[IO],
  val reuse:           Reusability[A],
  val client:          WebSocketClient[IO, S]
) extends ReactProps(LiveQueryRender.component)
    with LiveQueryRender.Props[IO, S, D, A]

object LiveQueryRender {
  trait Props[F[_], S, D, A] extends Render.LiveQuery.Props[F, Id, S, D, A]

  final case class State[F[_], S, D, A](
    queue:                   Queue[F, A],
    subscriptions:           NonEmptyList[GraphQLSubscription[F, _]],
    cancelConnectionTracker: CancelToken[F],
    renderer:                StreamRenderer.Component[A]
  ) extends Render.LiveQuery.State[F, Id, S, D, A]

  // Reusability should be controlled by enclosing components and reuse parameter. We allow rerender every time it's requested.
  implicit def propsReuse[F[_], S, D, A]: Reusability[Props[F, S, D, A]] = Reusability.never
  implicit def stateReuse[F[_], S, D, A]: Reusability[State[F, S, D, A]] = Reusability.never

  protected def componentBuilder[F[_], S, D, A] =
    ScalaComponent
      .builder[Props[F, S, D, A]]
      .initialState[Option[State[F, S, D, A]]](none)
      .render(Render.renderFn[F, Id, D, A](_))
      .componentDidMount(
        Render.LiveQuery
          .didMountFn[F, Id, S, D, A][Props[F, S, D, A], State[F, S, D, A]](
            "LiveQueryRender",
            (stream, props) => {
              implicit val F      = props.F
              implicit val logger = props.logger
              implicit val reuse  = props.reuse
              StreamRenderer.build(stream)
            },
            State.apply
          )
      )
      .componentWillUnmount(Render.LiveQuery.willUnmountFn[F, Id, S, D, A](_))
      .configure(Reusability.shouldComponentUpdate)
      .build

  val component = componentBuilder[IO, Any, Any, Any]
}
