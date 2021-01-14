// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.data.Validated
import cats.effect._
import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import fs2._
import lucuma.catalog.VoTableParser
import lucuma.core.enum.CatalogName
import lucuma.core.model.Target
import sttp.client3._

import scala.concurrent.duration._

object SimbadSearch {
  def search(term: NonEmptyString)(implicit cs: ContextShift[IO]): IO[Option[Target]] = {
    val backend  = FetchBackend()
    def httpCall =
      IO(
        basicRequest
          .post(
            uri"https://simbad.u-strasbg.fr/simbad/sim-id?Ident=${term}&output.format=VOTable"
          )
          .readTimeout(5.seconds)
          .send(backend)
      )

    IO.fromFuture(httpCall)
      .flatMap {
        _.body
          .traverse(
            Stream
              .emit[IO, String](_)
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
