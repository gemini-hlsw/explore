// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.data.Validated
import cats.effect._
import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.Constants
import lucuma.catalog.VoTableParser
import lucuma.core.enum.CatalogName
import lucuma.core.model.Target
import org.http4s._
import org.http4s.dom.FetchClientBuilder
import org.http4s.implicits._
import org.typelevel.log4cats.Logger
import retry._

import java.util.concurrent.TimeoutException
import scala.concurrent.duration._

object SimbadSearch {
  import RetryHelpers._

  def search[F[_]](
    term:       NonEmptyString,
    wildcard:   Boolean = false
  )(implicit F: Async[F], logger: Logger[F]): F[List[Target.Sidereal]] = {
    val baseURL =
      uri"https://simbad.u-strasbg.fr/simbad/sim-id"
        .withQueryParam("Ident", term.value)
        .withQueryParam("output.format", "VOTable")
        .withQueryParam("output.max", Constants.SimbadResultLimit)
    val url     =
      if (wildcard)
        baseURL
          .withQueryParam("NbIdent", "wild")
      else
        baseURL

    def isWorthRetrying(e: Throwable): F[Boolean] = e match {
      case _: TimeoutException => F.pure(!wildcard)
      case _                   => F.pure(true)
    }

    retryingOnSomeErrors(
      retryPolicy[F],
      isWorthRetrying,
      logError[F]("Simbad")
    ) {
      FetchClientBuilder[F]
        .withRequestTimeout(15.seconds)
        .resource
        .flatMap(_.run(Request[F](Method.POST, url)))
        .use {
          case Status.Successful(r) =>
            Logger[F].debug("Simbad search succeeded") >>
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
