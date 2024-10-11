// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.Pickler
import cats.Monoid
import cats.effect.Async
import cats.effect.Fiber
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.Sync
import cats.effect.std.Dispatcher
import cats.effect.syntax.all.*
import cats.syntax.all.*
import explore.model.boopickle.Boopickle.*
import log4cats.loglevel.LogLevelLogger
import org.scalajs.dom
import org.scalajs.dom.DedicatedWorkerGlobalScope
import org.typelevel.log4cats.Logger
import typings.loglevel.mod.LogLevelDesc

import WorkerMessage.*

/**
 * Implements the server side of a simple client/server protocol that provides a somewhat more
 * functional/effecful way of communicating with workers.
 */
trait WorkerServer[F[_]: Async, T: Pickler](using Monoid[F[Unit]]):
  protected val run: F[Unit] =
  (for {
    dispatcher      <- Dispatcher.parallel[F]
    given Logger[F] <- Resource.eval(setupLogger)
    _               <- Resource.eval(runInternal(dispatcher))
  } yield ()).useForever.void

  /**
   * Provide an interface to handlers with an incoming message and a method to send responses (which
   * can be invoked multiple times; the client will receive a `Stream` of responses).
   */
  protected case class Invocation(data: T, rawData: Pickled, respondRaw: Pickled => F[Unit]) {
    def respond[S: Pickler](value: S): F[Unit] = respondRaw(Pickled(asBytes(value)))
  }

  /**
   * Handle server-specific messages.
   */
  protected def handler: Logger[F] ?=> F[Invocation => F[Unit]]

  protected val F = summon[Sync[F]]

  protected def setupLogger: F[Logger[F]] = Sync[F].delay {
    LogLevelLogger.setLevel(LogLevelDesc.INFO)
    LogLevelLogger.createForRoot[F]
  }

  protected def mount(
    self:         DedicatedWorkerGlobalScope,
    handlerFn:    Invocation => F[Unit],
    cancelTokens: Ref[F, Map[WorkerProcessId, F[Unit]]]
  )(dispatcher: Dispatcher[F])(using Logger[F]): F[Unit] =
    F.delay(
      self.onmessage = (msg: dom.MessageEvent) =>
        dispatcher.unsafeRunAndForget(
          // Decode transferable events
          decodeFromTransferable[FromClient](msg)
            .map {
              case FromClient.ClientReady        =>
                postAsTransferable[F, FromServer](self, FromServer.ServerReady)
              case FromClient.Start(id, payload) =>
                F.delay(fromBytes[T](payload.value))
                  .rethrow
                  .flatMap(data =>
                    (
                      handlerFn(
                        Invocation(
                          data,
                          payload,
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
                        ) >> F.cede
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

  protected def runInternal(dispatcher: Dispatcher[F])(using Logger[F]): F[Unit] =
    for {
      self         <- F.delay(dom.DedicatedWorkerGlobalScope.self)
      handlerFn    <- handler
      cancelTokens <- Ref[F].of(Map.empty[WorkerProcessId, F[Unit]])
      _            <- Logger[F].debug("Mounting")
      _            <- mount(self, handlerFn, cancelTokens)(dispatcher)
      _            <- Logger[F].debug("Mounted, sending ready")
      // Because of racing conditions, the server may have missed the client's ClientReady
      // message by the time it initializes. So, we force send a ServerReady here just in case.
      // This assures that the client will get at least one ServerReady. We cannot just send
      // this one since the client may not be ready yet, so we must also send the one in
      // response to ClientReady above.
      _            <- postAsTransferable[F, FromServer](self, FromServer.ServerReady)
      _            <- Logger[F].debug("Ready sent!")
    } yield ()
