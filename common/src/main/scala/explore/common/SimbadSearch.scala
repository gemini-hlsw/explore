// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.data.Validated
import cats.effect._
import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.catalog.VoTableParser
import lucuma.core.enum.CatalogName
import lucuma.core.model.Target
import org.typelevel.log4cats.Logger
import retry._
import org.http4s._
import org.http4s.implicits._
import org.http4s.dom.FetchClient

// import scala.concurrent.duration._

object SimbadSearch {
  def search(term: NonEmptyString): IO[Option[Target]] =
    retryingOnAllErrors(retryPolicy[F], logError[F]("Simbad")) {
      FetchClient[IO]
        .run(
          Request[IO](
            Method.POST,
            uri"https://simbad.u-strasbg.fr/simbad/sim-id"
              .withQueryParam("Ident", term.value)
              .withQueryParam("output.format", "VOTable")
          )
        )
        .use {
          case Status.Successful(r) =>
            r.bodyText
              .through(VoTableParser.targets(CatalogName.Simbad))
              .compile
              .toList
              .map {
                _.collect { case Validated.Valid(t) => t }.headOption
              }
          case _                    => IO(none)
        }
    }

}
