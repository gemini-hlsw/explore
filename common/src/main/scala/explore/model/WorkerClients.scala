// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.effect.Async
import cats.effect.Resource
import cats.effect.std.Dispatcher
import cats.syntax.all._
import explore.events.*
import org.scalajs.dom
import workers.WorkerClient
import workers.WorkerClientBuilder

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

case class WorkerClients[F[_]](
  itc:     WorkerClient[F, ItcMessage.Request],
  catalog: WorkerClient[F, CatalogMessage.Request],
  ags:     WorkerClient[F, AgsMessage.Request]
)

object WorkerClients {

  /**
   * This deserves an explanation:
   *
   * To make the webworker act correctly in both dev and production we shoud import it as a module
   * rather than just doing a direct consructor call.
   *
   * Doing the import with the "worker" param gives a constructor for the worker which we can wrap
   * inline lets us save some space keeping a single chunk More info see:
   * https://vitejs.dev/guide/features.html#import-with-query-suffixes=
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

  def build[F[_]: Async](dispatcher: Dispatcher[F]): Resource[F, WorkerClients[F]] =
    (ItcClient.build[F](dispatcher),
     CatalogClient.build[F](dispatcher),
     AgsClient.build[F](dispatcher)
    ).mapN(WorkerClients.apply)
}
