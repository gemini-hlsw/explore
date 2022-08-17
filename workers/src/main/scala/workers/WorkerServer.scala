// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.Pickler
import cats.effect.IO
import cats.effect.kernel.Async
import cats.effect.kernel.Fiber
import cats.effect.kernel.Ref
import cats.effect.kernel.Resource
import cats.effect.kernel.Sync
import cats.effect.std.Dispatcher
import cats.effect.std.Dispatcher.apply
import cats.effect.syntax.all._
import cats.effect.unsafe.implicits._
import cats.kernel.Monoid
import cats.syntax.all._
import explore.model.boopickle.Boopickle._
import log4cats.loglevel.LogLevelLogger
import org.scalajs.dom
import org.typelevel.log4cats.Logger
import typings.loglevel.mod.LogLevelDesc

import scala.scalajs.js.annotation.JSExport

import WorkerMessage._

/**
 * Implements the server side of a simple client/server protocol that provides a somewhat more
 * functional/effecful way of communicating with workers.
 */
trait WorkerServer[F[_]: Async, T: Pickler](using Monoid[F[Unit]]) {
  val run: F[Unit] =
    (for {
      dispatcher      <- Dispatcher[F]
      given Logger[F] <- Resource.eval(setupLogger)
      _               <- Resource.eval(runInternal(dispatcher))
    } yield ()).useForever.void

    /**
     * Provide an interface to handlers with an incoming message and a method to send responses
     * (which can be invoked multiple times; the client will receive a `Stream` of responses).
     */
  protected final case class Invocation(data: T, protected val respondRaw: Pickled => F[Unit]) {
    def respond[S: Pickler](value: S): F[Unit] = respondRaw(Pickled(asBytes(value)))
  }

  /**
   * Handle server-specific messages.
   */
  protected def handler: Logger[F] ?=> F[Invocation => F[Unit]]

  private val F = summon[Sync[F]]

  protected def setupLogger: F[Logger[F]] = Sync[F].delay {
    LogLevelLogger.setLevel(LogLevelDesc.INFO)
    LogLevelLogger.createForRoot[F]
  }

  protected def runInternal(dispatcher: Dispatcher[F])(using Logger[F]): F[Unit] =
    (
      for {
        self         <- F.delay(dom.DedicatedWorkerGlobalScope.self)
        handlerFn    <- handler
        cancelTokens <- Ref[F].of(Map.empty[WorkerProcessId, F[Unit]])
      } yield self.onmessage = (msg: dom.MessageEvent) =>
        dispatcher.unsafeRunAndForget(
          // Decode transferable events
          decodeFromTransferable[FromClient](msg)
            .map {
              case FromClient.Start(id, payload) =>
                F.delay(fromBytes[T](payload.value))
                  .rethrow
                  .flatMap(data =>
                    (
                      handlerFn(
                        Invocation(
                          data,
                          pickled =>
                            postAsTransferable[F, FromServer](
                              self,
                              FromServer.Data(id, pickled)
                            ) >>
                              F.cede // This is important so that long-running processes don't hog the scheduler
                        )
                      ) >>
                        postAsTransferable[F, FromServer](self, FromServer.Complete(id))
                    )
                      .handleErrorWith(t =>
                        postAsTransferable[F, FromServer](
                          self,
                          FromServer.Error(id, WorkerException.fromThrowable(t))
                        )
                      )
                      .guarantee(cancelTokens.update(_ - id))
                      .start
                      .flatMap((fiber: Fiber[F, Throwable, Unit]) =>
                        cancelTokens.update(_ + (id -> fiber.cancel))
                      )
                  )
              case FromClient.End(id)            =>
                cancelTokens.modify { tokenMap =>
                  val token = tokenMap.get(id).orEmpty
                  (tokenMap - id, token)
                }.flatten
            }
            .orEmpty
            .handleErrorWith(e => Logger[F].error(e)("Error processing message in worker"))
        )
    ).handleErrorWith(e => Logger[F].error(e)("Error initializing worker"))
}
