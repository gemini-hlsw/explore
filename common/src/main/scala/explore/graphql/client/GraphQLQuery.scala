package explore.graphql.client

import io.circe.Decoder

trait GraphQLQuery {
  val document: String
  type Variables
  type Data

  case class Response(data: Data)
  implicit val jsonDecoder: Decoder[Response]
}
