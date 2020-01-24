package explore.graphql.client

import io.circe.{Encoder, Decoder}

trait GraphQLQuery {
  val document: String
  type Variables
  type Data

  implicit val varEncoder: Encoder[Variables]
  implicit val dataDecoder: Decoder[Data]
}
