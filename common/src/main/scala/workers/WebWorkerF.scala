// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic.*
import cats.effect.Async
import cats.effect.Resource
import cats.effect.Sync
import cats.effect.std.Dispatcher
import cats.syntax.all.*
import explore.model.boopickle.Boopickle.*
import fs2.Stream
import fs2.concurrent.Topic
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.typedarray.*

/**
 * WebWorker abstraction running on F. it is possible to post messages and get a stream of events
 * from the web worker
 */
trait WebWorkerF[F[_]] {

  /**
   * Post a message on effect F
   */
  def postMessage(message: js.Any): F[Unit]

  /**
   * Post a transferable message on effect F
   */
  def postTransferable(buffer: Int8Array): F[Unit]

  /**
   * Post a boopickle encoded message on effect F
   */
  def postTransferable[A: Pickler](a: A): F[Unit] =
    postTransferable(asTypedArray(a))

  /**
   * Terminate the web worker
   */
  def terminate: F[Unit]

  /**
   * Stream of events from the worker
   */
  def subscribe: Resource[F, Stream[F, dom.MessageEvent]]
}

object WebWorkerF {

  def apply[F[_]: Async](
    worker:     dom.Worker,
    dispatcher: Dispatcher[F]
  ): Resource[F, WebWorkerF[F]] =
    for {
      topic   <- Resource.make(Topic[F, dom.MessageEvent])(_.close.void)
      _       <-
        Resource.eval(
          Sync[F].delay(
            worker.onmessage =
              (e: dom.MessageEvent) => dispatcher.unsafeRunAndForget(topic.publish1(e))
          )
        )
      workerF <-
        Resource.make(Sync[F].delay(new WebWorkerF[F] {
          override def postMessage(message: js.Any): F[Unit] =
            Sync[F].delay(worker.postMessage(message))

          override def postTransferable(buffer: Int8Array): F[Unit] =
            Sync[F].delay(worker.postMessage(buffer, js.Array(buffer.buffer: dom.Transferable)))

          override val terminate: F[Unit] = Sync[F].delay(worker.terminate())

          // We want to use `subscribeAwait` instead of `subscribe`:
          // We usually want to send a query to the worker, which must be done once the subscription has
          // been initialized, so that the response is not lost.
          // `subscribe` performs the initialization within the `Stream` execution, so it provides no way to
          // synchronize at a point where we have a guarantee that it is actually reading new `Topic` messages.
          override val subscribe: Resource[F, Stream[F, dom.MessageEvent]] =
            topic.subscribeAwait(10)
        }))(w => w.terminate)
    } yield workerF

}
