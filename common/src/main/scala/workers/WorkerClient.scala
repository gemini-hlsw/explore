// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic.*
import cats.effect.Concurrent
import cats.effect.Deferred
import cats.effect.Resource
import cats.effect.Sync
import cats.effect.std.UUIDGen
import cats.effect.syntax.all.*
import cats.syntax.all.*
import explore.model.boopickle.Boopickle.*
import fs2.RaiseThrowable
import org.typelevel.log4cats.Logger

import WorkerMessage._

/**
 * Implements the client side of a simple client/server protocol that provides a somewhat more
 * functional/effecful way of communicating with workers.
 */
class WorkerClient[F[_]: Concurrent: UUIDGen: Logger, R: Pickler] private (
  worker:    WebWorkerF[F],
  initLatch: Deferred[F, Unit]
):
  private val waitForServer: F[Unit] =
    worker.streamResource
      .use(
        _.map(decodeFromTransferableEither[FromServer]).rethrow
          .collectFirst { case FromServer.Ready =>
            ()
          }
          .evalTap(_ => initLatch.complete(()))
          .compile
          .drain
      )
      .handleErrorWith(t => Logger[F].error(t)("Error initializing worker client"))
      .start
      .void

  /**
   * Make a request to the underlying worker and receive responses as a `Stream`.
   */
  def request[T <: R & WorkerRequest](requestMessage: T)(using
    Pickler[requestMessage.ResponseType]
  ): Resource[F, fs2.Stream[F, requestMessage.ResponseType]] =
    for {
      _      <- Resource.eval(initLatch.get) // Ensure server is initialized
      id     <- Resource.eval(UUIDGen.randomUUID).map(WorkerProcessId.apply)
      _      <- Resource.make(
                  Logger[F].debug(s">>> Starting request with id [$id]") >>
                    worker.postTransferable(
                      asTypedArray[FromClient](FromClient.Start(id, Pickled(asBytes[R](requestMessage))))
                    )
                )(_ =>
                  Logger[F].debug(s">>> Ending request with id [$id]") >>
                    worker.postTransferable(
                      asTypedArray[FromClient](FromClient.End(id))
                    )
                )
      stream <- worker.streamResource
    } yield stream
      .map(decodeFromTransferableEither[FromServer])
      .rethrow
      .evalTap(msg => Logger[F].debug(s"<<< Received msg from server [$msg]"))
      .collect {
        case FromServer.Data(mid, pickled) if mid === id =>
          fromBytes[requestMessage.ResponseType](pickled.value).some
        case FromServer.Complete(mid) if mid === id      =>
          none
        case FromServer.Error(mid, error) if mid === id  =>
          throw error
      }
      .unNoneTerminate
      .rethrow

    /**
     * Make a request to the underlying worker and receive a single response (if any) as the effect
     * result.
     */
  def requestSingle[T <: R & WorkerRequest](requestMessage: T)(using
    Pickler[requestMessage.ResponseType]
  ): F[Option[requestMessage.ResponseType]] = // TODO Should we implement a timeout here? Retry?
    request(requestMessage).use(_.head.compile.last)

  given Pickler[Nothing] =
    summon[Pickler[Unit]]
      .xmap(_ => throw new Exception("Attempted to unpickle Nothing"))(_ =>
        throw new Exception("Attempted to pickle Nothing")
      )

  def requestAndForget[T <: R & WorkerRequest](requestMessage: T)(using
    requestMessage.ResponseType =:= Nothing,
    Pickler[requestMessage.ResponseType]
  ): F[Unit] =
    request(requestMessage).use(_.compile.drain)

object WorkerClient:
  def fromWorker[F[_]: Concurrent: UUIDGen: Logger, R: Pickler](
    worker: WebWorkerF[F]
  ): Resource[F, WorkerClient[F, R]] =
    for {
      latch  <- Resource.eval(Deferred[F, Unit])
      client <- Resource.pure(new WorkerClient[F, R](worker, latch)).evalTap(_.waitForServer)
    } yield client
