// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic.*
import cats.effect.IO
import cats.effect.unsafe.implicits.*
import cats.syntax.all.*
import explore.events.AgsMessage
import explore.model.boopickle.CatalogPicklers.given
import lucuma.ags.Ags
import lucuma.ags.AgsAnalysis
import org.scalajs.dom
import org.typelevel.log4cats.Logger
import workers.*

import java.time.Duration
import java.time.Instant
import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("AgsServer", moduleID = "exploreworkers")
object AgsServer extends WorkerServer[IO, AgsMessage.Request] {
  @JSExport
  def runWorker(): Unit = run.unsafeRunAndForget()

  private val AgsCacheVersion: Int = 1

  private val CacheRetention: Duration = Duration.ofDays(60)

  def agsCalculation(r: AgsMessage.Request): IO[List[AgsAnalysis]] =
    IO.delay(
      Ags
        .agsAnalysis(r.constraints,
                     r.wavelength,
                     r.baseCoordinates,
                     r.position,
                     r.params,
                     r.candidates
        )
        .sorted(AgsAnalysis.rankingOrdering)
    )

  protected val handler: Logger[IO] ?=> IO[Invocation => IO[Unit]] =
    for
      self  <- IO(dom.DedicatedWorkerGlobalScope.self)
      cache <- Cache.withIDB[IO](self.indexedDB.toOption, "ags")
      _     <- cache.evict(CacheRetention).start
    yield invocation =>
      invocation.data match {
        case req @ AgsMessage.Request(_, _, _, _, _, _, _) =>
          val cacheableRequest =
            Cacheable(CacheName("ags"), CacheVersion(AgsCacheVersion), agsCalculation)
          (IO.now() >>= (now => cache.evict(now.minus(CacheRetention)).start)) >>
            cache.eval(cacheableRequest).apply(req).flatMap(m => invocation.respond(m))
      }
}
