// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.effect.*
import cats.syntax.all.*
import explore.modes.SpectroscopyModesMatrix
import lucuma.ui.utils.RetryHelpers.*
import org.http4s.*
import org.http4s.dom.FetchClientBuilder
import org.typelevel.log4cats.Logger
import retry.*

import scala.concurrent.duration.*

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
