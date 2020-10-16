// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.graphql

import cats.data.NonEmptyList
import cats.effect.ConcurrentEffect
import cats.effect.IO
import cats.syntax.all._
import clue.GraphQLStreamingClient
import crystal.react._
import crystal.react.implicits._
import fs2.concurrent.Queue
import io.chrisdavenport.log4cats.Logger
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.collections.message.Message
import react.semanticui.elements.icon.Icon
import react.semanticui.sizes._

final case class LiveQueryRender[D, A](
  query:               IO[D],
  extract:             D => A,
  changeSubscriptions: NonEmptyList[IO[GraphQLStreamingClient[IO, _]#Subscription[_]]]
)(
  val valueRender:     A => VdomNode,
  val pendingRender:   Long => VdomNode = (_ => Icon(name = "spinner", loading = true, size = Large)),
  val errorRender:     Throwable => VdomNode = (t => Message(error = true)(t.getMessage)),
  val onNewData:       IO[Unit] = IO.unit
)(implicit
  val ce:              ConcurrentEffect[IO],
  val logger:          Logger[IO],
  val reuse:           Reusability[A]
) extends ReactProps(LiveQueryRender.component)
    with LiveQueryRender.Props[IO, D, A]

object LiveQueryRender {
  trait Props[F[_], D, A] {
    val query: F[D]
    val extract: D => A
    val changeSubscriptions: NonEmptyList[F[GraphQLStreamingClient[F, _]#Subscription[_]]]

    val valueRender: A => VdomNode
    val pendingRender: Long => VdomNode
    val errorRender: Throwable => VdomNode
    val onNewData: F[Unit]

    implicit val ce: ConcurrentEffect[F]
    implicit val logger: Logger[F]
    implicit val reuse: Reusability[A]
  }

  final case class State[F[_], D, A](
    queue:         Queue[F, A],
    subscriptions: NonEmptyList[GraphQLStreamingClient[F, _]#Subscription[_]],
    renderer:      StreamRenderer.Component[A]
  )

  // Reusability should be controlled by enclosing components and reuse parameter. We allow rerender every time it's requested.
  implicit def propsReuse[F[_], D, A]: Reusability[Props[F, D, A]] = Reusability.never
  implicit def stateReuse[F[_], D, A]: Reusability[State[F, D, A]] = Reusability.never

  protected def componentBuilder[F[_], D, A] =
    ScalaComponent
      .builder[Props[F, D, A]]
      .initialState[Option[State[F, D, A]]](none)
      .render { $ =>
        println(s"RENDERING WITH STATE: [${$.state}]")

        React.Fragment(
          $.state.fold[VdomNode](EmptyVdom)(
            _.renderer(_.fold($.props.pendingRender, $.props.errorRender, $.props.valueRender))
          )
        )
      }
      .componentDidMount { $ =>
        implicit val ce     = $.props.ce
        implicit val logger = $.props.logger
        implicit val reuse  = $.props.reuse

        def queryAndEnqueue(queue: Queue[F, A]): F[Unit] =
          for {
            result <- $.props.query.map($.props.extract)
            _      <- queue.enqueue1(result)
            _      <- $.props.onNewData
          } yield ()

        val init =
          for {
            queue         <- Queue.unbounded[F, A]
            subscriptions <- $.props.changeSubscriptions.sequence
            _             <- queryAndEnqueue(queue)
            _             <- subscriptions
                               .map(_.stream.evalMap(_ => queryAndEnqueue(queue)).compile.drain)
                               .sequence
            renderer       = StreamRenderer.build(queue.dequeue)
            _             <- $.setStateIn[F](State(queue, subscriptions, renderer).some)
          } yield ()

        init.handleErrorWith(t => logger.error(t)("Error initializing LiveQueryRender")).runInCB
      }
      .componentWillUnmount { $ =>
        implicit val ce = $.props.ce

        $.state.map(_.subscriptions.map(_.stop()).sequence.void.runInCB).getOrEmpty
      }
      .configure(Reusability.shouldComponentUpdate)
      .build

  val component = componentBuilder[IO, Any, Any]
}
