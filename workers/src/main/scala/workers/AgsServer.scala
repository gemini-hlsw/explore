// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import boopickle.DefaultBasic.*
import cats.effect.IO
import cats.effect.unsafe.implicits._
import explore.events.AgsMessage
import explore.model.boopickle.CatalogPicklers.given
import lucuma.ags.Ags
import lucuma.ags.AgsAnalysis
import org.typelevel.log4cats.Logger

import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.annotation.JSExportTopLevel

@JSExportTopLevel("AgsServer", moduleID = "exploreworkers")
object AgsServer extends WorkerServer[IO, AgsMessage.Request] {
  @JSExport
  def runWorker(): Unit = run.unsafeRunAndForget()

  protected val handler: Logger[IO] ?=> IO[Invocation => IO[Unit]] = IO { invocation =>
    invocation.data match {
      case AgsMessage.Request(id, constraints, wavelength, base, basePos, params, candidates) =>
        Logger[IO].debug(s"AGS request for $id") >>
          IO.delay(
            Ags
              .agsAnalysis(constraints, wavelength, base, basePos, params, candidates)
              .sorted(AgsAnalysis.rankingOrdering)
          ).flatMap(r =>
            Logger[IO].debug(s"AGS response for $id") >>
              invocation.respond(r)
          )
    }
  }
}
