// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.effect._
import cats.syntax.all._
import explore.common.RetryHelpers._
import explore.modes.SpectroscopyModesMatrix
import org.http4s._
import org.http4s.dom.FetchClientBuilder
import org.typelevel.log4cats.Logger
import retry._

import scala.concurrent.duration._

object StaticData {
  def build[F[_]: Async: Logger](spectroscopyMatrixUri: Uri): F[SpectroscopyModesMatrix] = {
    val client = FetchClientBuilder[F]
      .withRequestTimeout(5.seconds)
      .create

    val spectroscopyMatrix =
      retryingOnAllErrors(retryPolicy[F], logError[F]("Spectroscopy Matrix")) {
        client.run(Request(Method.GET, spectroscopyMatrixUri)).use {
          case Status.Successful(r) =>
            SpectroscopyModesMatrix[F](r.bodyText)
          case fail                 =>
            // If fetching fails, do we want to continue without the matrix, or do we want to crash?
            Logger[F].warn(
              s"Could not retrieve spectroscopy matrix. Code [${fail.status.code}] - Body: [${fail.as[String]}]"
            ) >> SpectroscopyModesMatrix.empty.pure[F]
        }
      }
    spectroscopyMatrix
  }
}
