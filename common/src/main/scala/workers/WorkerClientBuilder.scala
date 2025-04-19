// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.Pickler
import cats.effect.Async
import cats.effect.Resource
import cats.effect.std.Dispatcher
import cats.effect.std.UUIDGen
import org.scalajs.dom
import org.typelevel.log4cats.Logger

trait WorkerClientBuilder[R: Pickler](worker: dom.Worker):
  def build[F[_]: Async: UUIDGen: Logger](
    dispatcher: Dispatcher[F]
  ): Resource[F, WorkerClient[F, R]] =
    for {
      worker <- WebWorkerF[F](worker, dispatcher)
      client <- WorkerClient.fromWorker(worker)
    } yield client

  inline def apply[F[_]](using ev: WorkerClient[F, R]): WorkerClient[F, R] = ev
