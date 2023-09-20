// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic.*
import cats.effect.*
import cats.effect.unsafe.implicits.*
import cats.syntax.all.*
import explore.events.*
import explore.itc.ITCGraphRequests
import explore.itc.ITCRequests
import explore.itc.ITCVersionsRequests
import explore.model.StaticData
import explore.model.boopickle.ItcPicklers
import explore.modes.SpectroscopyModesMatrix
import lucuma.itc.client.ItcClient
import org.http4s.client.Client
import org.http4s.dom.FetchClientBuilder
import org.scalajs.dom
import org.typelevel.log4cats.Logger

import java.time.Duration
import scala.concurrent.duration.*
import scala.scalajs.js

import js.annotation.*

/**
 * Web worker that can query gaia and store results locally
 */
@JSExportTopLevel("ItcServer", moduleID = "exploreworkers")
object ItcServer extends WorkerServer[IO, ItcMessage.Request] with ItcPicklers {
  @JSExport
  def runWorker(): Unit = run.unsafeRunAndForget()

  private val CacheRetention: Duration = Duration.ofDays(30)

  private def createClient[F[_]: Async]: Client[F] =
    FetchClientBuilder[F]
      .withRequestTimeout(5.seconds)
      .create

  protected val handler: Logger[IO] ?=> IO[Invocation => IO[Unit]] =
    for {
      self      <- IO(dom.DedicatedWorkerGlobalScope.self)
      cache     <- Cache.withIDB[IO](self.indexedDB.toOption, "explore-itc")
      _         <- cache.evict(CacheRetention).start
      matrix    <- Deferred[IO, SpectroscopyModesMatrix]
      itcClient <- Deferred[IO, ItcClient[IO]]
    } yield { invocation =>
      invocation.data match
        case ItcMessage.Initialize(itcURI) =>
          for {
            client <- ItcClient.create[IO](itcURI, createClient)
            _      <- itcClient.complete(client).void
            _      <- ITCVersionsRequests.queryItc[IO](cache, client).andWait(1.hour).foreverM.start
          } yield ()

        case ItcMessage.CleanCache =>
          cache.clear *> invocation.respond(())

        case ItcMessage.SpectroscopyMatrixRequest(uri) =>
          matrix.tryGet.flatMap {
            case Some(m) =>
              Logger[IO].debug("ITC matrix load from memory") *>
                invocation.respond(m)
            case _       =>
              Logger[IO].debug("ITC matrix load from remote") *>
                StaticData.build[IO](uri).flatMap { m =>
                  matrix.complete(m) *> invocation.respond(m)
                }
          }

        case ItcMessage.Query(
              wavelength,
              signalToNoise,
              constraint,
              targets,
              rows,
              signalToNoiseAt
            ) =>
          Logger[IO].debug(s"ITC query ${rows.length}") >>
            itcClient.get >>= (implicit client =>
            ITCRequests
              .queryItc[IO](
                wavelength,
                signalToNoise,
                constraint,
                targets,
                rows,
                signalToNoiseAt,
                cache,
                r => invocation.respond(r)
              )
          )

        case ItcMessage.GraphQuery(wavelength,
                                   exposureTime,
                                   exposures,
                                   signalToNoiseAt,
                                   constraint,
                                   targets,
                                   mode
            ) =>
          Logger[IO].debug(s"ITC graph query ${mode}") >>
            itcClient.get >>= (implicit client =>
            ITCGraphRequests
              .queryItc[IO](
                wavelength,
                exposureTime,
                exposures,
                signalToNoiseAt,
                constraint,
                targets,
                mode,
                cache,
                r => invocation.respond(r)
              )
          )
    }
}
