// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.Default._
import cats.effect.Async
import cats.effect.Resource
import cats.effect.Sync
import cats.effect.std.Dispatcher
import explore.model.boopickle._
import fs2.Stream
import fs2.concurrent.Channel
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.typedarray._

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
   * Post a transferrable message on effect F
   */
  def postTransferrable(buffer: Int8Array): F[Unit]

  /**
   * Post a boopickle encoded message on effect F
   */
  def postTransferrable[A: Pickler](a: A): F[Unit] =
    postTransferrable(asTransferable(a))

  /**
   * Terminate the web worker
   */
  def terminate: F[Unit]

  /**
   * Streams of events from the worker
   */
  def stream: Stream[F, dom.MessageEvent]
}

object WebWorkerF {

  def apply[F[_]: Async](
    worker:     dom.Worker,
    dispatcher: Dispatcher[F]
  ): Resource[F, WebWorkerF[F]] =
    Resource.make(Sync[F].delay(new WebWorkerF[F] {
      def postMessage(message: js.Any): F[Unit] =
        Sync[F].delay(worker.postMessage(message))

      def postTransferrable(buffer: Int8Array): F[Unit] =
        Sync[F].delay(worker.postMessage(buffer, js.Array(buffer.buffer: dom.Transferable)))

      def terminate: F[Unit] =
        Sync[F].delay(worker.terminate())

      def stream: Stream[F, dom.MessageEvent] =
        for {
          channel <- Stream.eval(Channel.unbounded[F, dom.MessageEvent])
          _       <- Stream.eval(
                       Sync[F].delay(worker.onmessage =
                         (e: dom.MessageEvent) => dispatcher.unsafeRunAndForget(channel.send(e))
                       )
                     )
          stream  <- channel.stream
        } yield stream
    }))(w => w.terminate)
}
