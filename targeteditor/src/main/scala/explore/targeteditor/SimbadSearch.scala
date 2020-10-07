package explore.targeteditor

import scala.concurrent.duration._

import cats.data.Validated
import cats.effect._
import cats.implicits._
import eu.timepit.refined.types.string.NonEmptyString
import fs2._
import lucuma.catalog.VoTableParser
import lucuma.core.enum.CatalogName
import lucuma.core.model.Target
import sttp.client3._

object SimbadSearch {
  def search(term: NonEmptyString)(implicit cs: ContextShift[IO]): IO[Option[Target]] = {
    val backend  = FetchBackend()
    def response = basicRequest
      .post(
        uri"https://simbad.u-strasbg.fr/simbad/sim-id?Ident=${term}&output.format=VOTable"
      )
      .readTimeout(5.seconds)
      .send(backend)

    IO.fromFuture(IO(response))
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
