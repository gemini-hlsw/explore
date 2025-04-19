// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import _root_.boopickle.DefaultBasic.*
import cats.FlatMap
import cats.effect.Async
import cats.effect.Resource
import cats.effect.std.Dispatcher
import cats.effect.std.SecureRandom
import cats.syntax.all.*
import explore.events.*
import org.scalajs.dom
import org.typelevel.log4cats.Logger
import workers.WorkerClient
import workers.WorkerClientBuilder

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

case class WorkerClients[F[_]](
  itc:     WorkerClient[F, ItcMessage.Request],
  catalog: WorkerClient[F, CatalogMessage.Request],
  ags:     WorkerClient[F, AgsMessage.Request],
  plot:    WorkerClient[F, PlotMessage.Request]
) {
  def clearAll(andThen: F[Unit])(using FlatMap[F]): F[Unit] =
    for {
      _ <- itc.requestSingle(ItcMessage.CleanCache)
      _ <- plot.requestSingle(PlotMessage.CleanCache)
      _ <- ags.requestSingle(AgsMessage.CleanCache)
      _ <- catalog.requestSingle(CatalogMessage.CleanCache)
      _ <- andThen
    } yield ()
}

object WorkerClients {

  /**
   * This deserves an explanation:
   *
   * To make the webworker act correctly in both dev and production we shoud import it as a module
   * rather than just doing a direct consructor call.
   *
   * Doing the import with the "worker" param gives a constructor for the worker which we can wrap
   * inline and lets us save some space keeping a single chunk. More info see:
   * https://vitejs.dev/guide/features.html#import-with-query-suffixes
   *
   * Note that we don't use "&inline" since it increases the output size significantly.
   */
  @js.native
  @JSImport("/itcworker.js?worker", JSImport.Default)
  private object ItcWorker extends js.Object {
    def apply(): dom.Worker = js.native
  }

  object ItcClient extends WorkerClientBuilder[ItcMessage.Request](ItcWorker())

  @js.native
  @JSImport("/agsworker.js?worker", JSImport.Default)
  private object AgsWorker extends js.Object {
    def apply(): dom.Worker = js.native
  }

  object AgsClient extends WorkerClientBuilder[AgsMessage.Request](AgsWorker())

  @js.native
  @JSImport("/catalogworker.js?worker", JSImport.Default)
  private object CatalogWorker extends js.Object {
    def apply(): dom.Worker = js.native
  }

  object CatalogClient extends WorkerClientBuilder[CatalogMessage.Request](CatalogWorker())

  @js.native
  @JSImport("/plotworker.js?worker", JSImport.Default)
  private object PlotWorker extends js.Object {
    def apply(): dom.Worker = js.native
  }

  object PlotClient extends WorkerClientBuilder[PlotMessage.Request](PlotWorker())

  def build[F[_]: Async: Logger: SecureRandom](
    dispatcher: Dispatcher[F]
  ): Resource[F, WorkerClients[F]] =
    (ItcClient.build[F](dispatcher),
     CatalogClient.build[F](dispatcher),
     AgsClient.build[F](dispatcher),
     PlotClient.build[F](dispatcher)
    ).parMapN(WorkerClients.apply)
}
