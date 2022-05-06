// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import cats.effect.Async
import cats.effect.Sync
import cats.effect.std.Dispatcher
import fs2.Stream
import fs2.concurrent.Channel
import org.scalajs.dom

import scala.scalajs.js

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
   * Terminate the web worker
   */
  def terminate: F[Unit]

  /**
   * Streams of events from the worker
   */
  def stream: Stream[F, dom.MessageEvent]
}

object WebWorkerF {

  def apply[F[_]: Async](worker: dom.Worker) = new WebWorkerF[F] {
    def postMessage(message: js.Any): F[Unit] = Sync[F].delay(worker.postMessage(message))

    def terminate: F[Unit] = Sync[F].delay(worker.terminate())

    def stream: Stream[F, dom.MessageEvent] =
      for {
        dispatcher <- Stream.resource(Dispatcher[F])
        channel    <- Stream.eval(Channel.unbounded[F, dom.MessageEvent])
        _          <- Stream.eval(
                        Sync[F].delay(worker.onmessage =
                          (e: dom.MessageEvent) =>
                            dispatcher.unsafeRunAndForget(
                              channel
                                .send(e)
                            )
                        )
                      )
        stream     <- channel.stream
      } yield stream
  }
}
