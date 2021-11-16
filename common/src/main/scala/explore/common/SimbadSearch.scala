// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.data.Validated
import cats.effect._
import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.catalog.VoTableParser
import lucuma.core.enum.CatalogName
import lucuma.core.model.SiderealTarget
import org.http4s._
import org.http4s.dom.FetchClientBuilder
import org.http4s.implicits._
import org.typelevel.log4cats.Logger
import retry._

import scala.concurrent.duration._

object SimbadSearch {
  import RetryHelpers._

  def search[F[_]: Async: Logger](
    term:     NonEmptyString,
    wildcard: Boolean = false
  ): F[List[SiderealTarget]] = {
    val baseURL =
      uri"https://simbad.u-strasbg.fr/simbad/sim-id"
        .withQueryParam("Ident", term.value)
        .withQueryParam("output.format", "VOTable")
    val url     =
      if (wildcard)
        baseURL.withQueryParam("NbIdent", "wild")
      else
        baseURL

    retryingOnAllErrors(retryPolicy[F], logError[F]("Simbad")) {
      FetchClientBuilder[F]
        .withRequestTimeout(20.seconds)
        .resource
        .flatMap(_.run(Request[F](Method.POST, url)))
        .use {
          case Status.Successful(r) =>
            r.bodyText
              .through(VoTableParser.targets(CatalogName.Simbad))
              .compile
              .toList
              .map {
                _.collect { case Validated.Valid(t) => t }
              }
          case _                    =>
            Logger[F].error(s"Simbad search failed for term [$term]").as(List.empty)
        }
    }
  }
}
