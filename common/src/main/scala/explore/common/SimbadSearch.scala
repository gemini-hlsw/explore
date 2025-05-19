// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.effect.*
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.Constants
import lucuma.catalog.CatalogTargetResult
import lucuma.catalog.votable.CatalogAdapter
import lucuma.catalog.votable.CatalogSearch
import org.http4s.*
import org.http4s.dom.FetchClientBuilder
import org.http4s.implicits.*
import org.typelevel.log4cats.Logger
import retry.*

import java.util.concurrent.TimeoutException
import scala.concurrent.duration.*

object SimbadSearch {
  import RetryHelpers.*

  def search[F[_]](
    term:     NonEmptyString,
    wildcard: Boolean = false
  )(implicit F: Async[F], logger: Logger[F]): F[List[CatalogTargetResult]] =
    val harvardUrl = uri"https://simbad.cfa.harvard.edu/simbad/sim-id"
    val strasbgUrl = uri"https://simbad.u-strasbg.fr/simbad/sim-id"
    // Try both harvard and strasbourg and return whichever completes first.
    // If a site is down (as has been known to happen with harvard), the one that is up
    /// will complete. I thought I would have to use racePair to handle errors, but it
    // seems to work as is.
    F.race(searchSingle(harvardUrl, term, wildcard), searchSingle(strasbgUrl, term, wildcard)).map {
      case Left(results)  => results
      case Right(results) => results
    }

  private def searchSingle[F[_]](
    simbadUrl: Uri,
    term:      NonEmptyString,
    wildcard:  Boolean = false
  )(implicit F: Async[F], logger: Logger[F]): F[List[CatalogTargetResult]] = {
    val baseURL =
      simbadUrl
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
                .through(CatalogSearch.siderealTargets(CatalogAdapter.Simbad))
                .compile
                .toList
                .map {
                  _.collect { case Right(r) => r }
                }
          case _                    =>
            Logger[F].error(s"Simbad search failed for term [$term]").as(List.empty)
        }
    }
  }
}
