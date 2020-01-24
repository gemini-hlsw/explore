package explore.graphql

import client.GraphQLQuery
import explore.model.Task
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }

object AddMutation extends GraphQLQuery {
  val document = """
      mutation AddMutation($title: String!) {
        add(title: $title) {
          id
          title
          completed
        }
      }"""

  case class Variables(title: String)
  object Variables { implicit val jsonEncoder: Encoder[Variables] = deriveEncoder[Variables] }

  case class Data(add: Option[Task])
  object Data { implicit val jsonDecoder: Decoder[Data] = deriveDecoder[Data] }

  implicit val varEncoder: Encoder[Variables] = Variables.jsonEncoder
  implicit val dataDecoder: Decoder[Data] = Data.jsonDecoder
}