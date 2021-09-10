// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.data.Validated
import cats.effect._
import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import fs2._
import lucuma.catalog.VoTableParser
import lucuma.core.enum.CatalogName
import lucuma.core.model.Target
import org.typelevel.log4cats.Logger
import retry._
import sttp.client3._
import sttp.client3.impl.cats.FetchCatsBackend

import scala.concurrent.duration._

object SimbadSearch {
  import RetryHelpers._

  def search[F[_]: Async: Logger](term: NonEmptyString): F[Option[Target]] =
    retryingOnAllErrors(retryPolicy[F], logError[F]("Simbad")) {
      val backend  = FetchCatsBackend[F]()
      def httpCall =
        basicRequest
          .post(
            uri"https://simbad.u-strasbg.fr/simbad/sim-id?Ident=$term&output.format=VOTable"
          )
          .readTimeout(5.seconds)
          .send(backend)

      httpCall
        .flatMap {
          _.body
            .traverse(
              Stream
                .emit[F, String](_)
                .through(VoTableParser.targets(CatalogName.Simbad))
                .compile
                .toList
                .map {
                  _.collect { case Validated.Valid(t) => t }.headOption
                }
            )
            .map {
              case Right(Some((t))) => t.some
              case _                => none
            }
        }
    }

}
