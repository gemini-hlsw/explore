// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic._
import cats.effect.Concurrent
import cats.effect.Resource
import cats.effect.Sync
import cats.effect.std.UUIDGen
import cats.syntax.all._
import explore.model.boopickle.Boopickle._
import fs2.RaiseThrowable

import WorkerMessage._

/**
 * Implements the client side of a simple client/server protocol that provides a somewhat more
 * functional/effecful way of communicating with workers.
 */
class WorkerClient[F[_]: Concurrent: UUIDGen, R: Pickler](worker: WebWorkerF[F]) {

  /**
   * Make a request to the underlying worker and receive responses as a `Stream`.
   */
  def request[T <: R & WorkerRequest](requestMessage: T)(using
    Pickler[requestMessage.ResponseType]
  ): Resource[F, fs2.Stream[F, requestMessage.ResponseType]] =
    for {
      id     <- Resource.eval(UUIDGen.randomUUID).map(WorkerProcessId.apply)
      _      <- Resource.make(
                  worker.postTransferable(
                    asTypedArray[FromClient](FromClient.Start(id, Pickled(asBytes[R](requestMessage))))
                  )
                )(_ => worker.postTransferable(asTypedArray[FromClient](FromClient.End(id))))
      stream <- worker.streamResource
    } yield stream
      .map(decodeFromTransferableEither[FromServer])
      .rethrow
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

  given Pickler[Nothing] = ??? // Intentionally left unimplemented, should never be called

  def invoke[T <: R & WorkerRequest](requestMessage: T)(using
    requestMessage.ResponseType =:= Nothing,
    Pickler[requestMessage.ResponseType]
  ): F[Unit] =
    request(requestMessage).use(_.compile.drain)
}
