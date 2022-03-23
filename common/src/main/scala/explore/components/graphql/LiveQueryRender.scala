// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.graphql

import cats.Id
import cats.effect.Async
import cats.effect.IO
import cats.effect.std.Queue
import cats.syntax.all._
import clue.GraphQLSubscription
import clue.WebSocketClient
import crystal.Pot
import crystal.react._
import crystal.react.reuse._
import japgolly.scalajs.react._
import japgolly.scalajs.react.util.Effect
import japgolly.scalajs.react.vdom.html_<^._
import org.typelevel.log4cats.Logger
import react.common._
import scala.reflect.ClassTag

final case class LiveQueryRender[S, D, A](
  query:               Reuse[IO[D]],
  extract:             D ==> A,
  changeSubscriptions: Reuse[List[IO[GraphQLSubscription[IO, _]]]]
)(
  val render:          Pot[A] ==> VdomNode,
  val onNewData:       Reuse[IO[Unit]] = Reuse.always(IO.unit)
)(implicit
  val F:               Async[IO],
  val dispatcher:      Effect.Dispatch[IO],
  val logger:          Logger[IO],
  val classTag:        ClassTag[A],
  val reuse:           Reusability[A],
  val client:          WebSocketClient[IO, S]
) extends ReactProps(LiveQueryRender.component)
    with LiveQueryRender.Props[IO, S, D, A]

object LiveQueryRender {
  trait Props[F[_], S, D, A] extends Render.LiveQuery.Props[F, Id, S, D, A]

  final case class State[F[_], S, D, A](
    queue:                   Queue[F, A],
    subscriptions:           List[GraphQLSubscription[F, _]],
    cancelConnectionTracker: F[Unit],
    renderer:                StreamRenderer.Component[A]
  ) extends Render.LiveQuery.State[F, Id, S, D, A]

  implicit def propsReuse[F[_], S, D, A]: Reusability[Props[F, S, D, A]] =
    Reusability.by(p => (p.query, p.extract, p.changeSubscriptions, p.render, p.onNewData))
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
              import props._

              StreamRenderer.build(stream.map(a => Reuse(a).self))
            },
            State.apply
          )
      )
      .componentWillUnmount(Render.LiveQuery.willUnmountFn[F, Id, S, D, A](_))
      .configure(Reusability.shouldComponentUpdate)
      .build

  val component = componentBuilder[IO, Any, Any, Any]
}
