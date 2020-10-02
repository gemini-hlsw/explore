package explore.targeteditor

import cats.data.Validated
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.Target
import lucuma.core.enum.CatalogName
import lucuma.catalog.VoTableParser
import cats.effect._
import cats.implicits._
import fs2._
import sttp.client3._
import scala.concurrent.duration._

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
              .last
          )
          .map {
            case Right(Some(Validated.Valid(t))) =>
              println(s" SIMBAD: $t")
              t.some
            case e                               =>
              println(s" SIMBAD: $e")
              none
          }
      }

  }

}
