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
import explore.model.boopickle.ItcPicklers
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
      .withRequestTimeout(10.seconds)
      .create

  protected val handler: Logger[IO] ?=> IO[Invocation => IO[Unit]] =
    for {
      self      <- IO(dom.DedicatedWorkerGlobalScope.self)
      cache     <- Cache.withIDB[IO](self.indexedDB.toOption, "explore-itc")
      _         <- cache.evict(CacheRetention).start
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

        case ItcMessage.Query(
              wavelength,
              signalToNoise,
              constraint,
              targets,
              rows
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
                cache,
                r => invocation.respond(r)
              )
          )

        case ItcMessage.GraphQuery(
              wavelength,
              signalToNoise,
              constraint,
              targets,
              mode
            ) =>
          Logger[IO].debug(s"ITC graph query ${mode}") >>
            itcClient.get >>= (implicit client =>
            ITCGraphRequests
              .queryItc[IO](
                wavelength,
                signalToNoise,
                constraint,
                targets,
                mode,
                cache,
                r => invocation.respond(r)
              )
          )
    }
}
