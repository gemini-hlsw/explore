package explore.graphql.client

import cats.effect._
import org.scalajs.dom.ext.Ajax
import io.circe.Encoder
import io.circe.generic.semiauto.deriveEncoder
import io.circe.syntax._
import io.circe.parser.decode

case class AjaxQuery(query: String)
object AjaxQuery {
    implicit val jsonEncoder: Encoder[AjaxQuery] = deriveEncoder[AjaxQuery]
}

case class AjaxIOGraphQLClient(uri: String)(implicit csIO: ContextShift[IO]) extends GraphQLClient[IO] {
    def query(query: GraphQLQuery)(variables: Option[query.Variables] = None): IO[query.Data] = 
        IO.fromFuture(IO(
            Ajax.post(
                url = uri,
                data = AjaxQuery(query.document).asJson.toString,
                headers = Map("Content-Type" -> "application/json")
                )
        ))
        .map(r => decode[query.Response](r.responseText)(query.jsonDecoder))
        .flatMap(IO.fromEither)
        .map(_.data)
}
