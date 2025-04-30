// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic.*
import cats.effect.IO
import cats.effect.unsafe.implicits.*
import explore.events.AgsMessage
import explore.model.boopickle.CatalogPicklers.given
import lucuma.ags.Ags
import lucuma.ags.AgsAnalysis
import org.scalajs.dom
import org.typelevel.log4cats.Logger
import workers.*

import java.time.Duration
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("AgsServer", moduleID = "exploreworkers")
object AgsServer extends WorkerServer[IO, AgsMessage.Request] {
  @JSExport
  def runWorker(): Unit = run.unsafeRunAndForget()

  private val AgsCacheVersion: Int = 18

  private val CacheRetention: Duration = Duration.ofDays(60)

  def agsCalculation(r: AgsMessage.AgsRequest): IO[List[AgsAnalysis.Usable]] =
    IO.blocking(
      Ags
        .agsAnalysis(r.constraints,
                     r.wavelength,
                     r.baseCoordinates,
                     r.scienceCoordinates,
                     r.positions,
                     r.params,
                     r.candidates
        )
        .sortUsablePositions
    )

  protected val handler: Logger[IO] ?=> IO[Invocation => IO[Unit]] =
    for
      self  <- IO(dom.DedicatedWorkerGlobalScope.self)
      cache <- Cache.withIDB[IO](self.indexedDB.toOption, "ags")
      _     <- cache.evict(CacheRetention).start
    yield invocation =>
      invocation.data match {
        case AgsMessage.CleanCache                               =>
          cache.clear *> invocation.respond(())
        case req @ AgsMessage.AgsRequest(_, _, _, _, _, _, _, _) =>
          val cacheableRequest =
            Cacheable(CacheName("ags"), CacheVersion(AgsCacheVersion), agsCalculation)
          cache
            .eval(cacheableRequest)
            .apply(req)
            .flatMap(invocation.respond)
      }
}
