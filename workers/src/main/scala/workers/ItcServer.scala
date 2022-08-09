// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic._
import cats.effect._
import cats.effect.unsafe.implicits._
import cats.syntax.all._
import clue.TransactionalClient
import clue._
import clue.js.FetchJSBackend
import clue.js.FetchMethod
import explore.events._
import explore.itc.ITCGraphRequests
import explore.itc.ITCRequests
import explore.model.AppConfig
import explore.model.StaticData
import explore.model.boopickle.Boopickle._
import explore.model.boopickle.ItcPicklers
import explore.modes.SpectroscopyModesMatrix
import japgolly.scalajs.react.callback.CallbackCatsEffect._
import japgolly.scalajs.react.callback._
import log4cats.loglevel.LogLevelLogger
import lucuma.ags.Ags
import lucuma.ags.AgsAnalysis
import org.http4s.dom.FetchClientBuilder
import org.scalajs.dom
import org.typelevel.log4cats.Logger
import queries.schemas.ITC
import typings.loglevel.mod.LogLevelDesc

import java.time.Duration
import scala.concurrent.duration._
import scala.scalajs.js

import js.annotation._

trait AsyncToIO {
  class AsyncCallbackOps[A](val a: AsyncCallback[A]) {
    def toIO: IO[A] = asyncCallbackToIO.apply(a)
  }

  implicit def AsyncTo[A](a: AsyncCallback[A]): AsyncCallbackOps[A] =
    new AsyncCallbackOps(a)
}

/**
 * Web worker that can query gaia and store results locally
 */
@JSExportTopLevel("ItcServer", moduleID = "exploreworkers")
object ItcServer extends WorkerServer[IO, ItcMessage.Request] with ItcPicklers with AsyncToIO {
  @JSExport
  def runWorker(): Unit = run.unsafeRunAndForget()

  private def fetchConfig[F[_]: Async]: F[AppConfig] =
    // We want to avoid caching the static server redirect and the config files (they are not fingerprinted by vite).
    AppConfig.fetchConfig(
      FetchClientBuilder[F]
        .withRequestTimeout(5.seconds)
        .withCache(dom.RequestCache.`no-store`)
        .create
    )

  protected val handler: Logger[IO] ?=> IO[Invocation => IO[Unit]] =
    for {
      matrix                             <- Deferred[IO, SpectroscopyModesMatrix]
      config                             <- fetchConfig[IO]
      given TransactionalClient[IO, ITC] <- {
        given TransactionalBackend[IO] = FetchJSBackend[IO](FetchMethod.GET)
        TransactionalClient.of[IO, ITC](config.itcURI, "ITC")
      }
    } yield { invocation =>
      invocation.data match {
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

        case ItcMessage.Query(wavelength, signalToNoise, constraint, targets, rows) =>
          Logger[IO].debug(s"ITC query ${rows.length}") *>
            ITCRequests
              .queryItc[IO](
                wavelength,
                signalToNoise,
                constraint,
                targets,
                rows,
                r => invocation.respond(r)
                // r => invocation.respond(ItcMessage.QueryResult(id, r))
              )

        case ItcMessage.GraphQuery(wavelength, signalToNoise, constraint, targets, mode) =>
          Logger[IO].debug(s"ITC graph query ${mode}") *>
            ITCGraphRequests
              .queryItc[IO](
                wavelength,
                signalToNoise,
                constraint,
                targets,
                mode,
                r => invocation.respond(r)
                // r => invocation.respond(ItcMessage.GraphResult(r))
              )
      }
    }
}
