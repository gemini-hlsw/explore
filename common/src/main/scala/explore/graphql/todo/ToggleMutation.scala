package explore.graphql

import client.GraphQLQuery
import explore.model.Task
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }

object ToggleMutation extends GraphQLQuery {
  val document = """
      mutation ToggleMutation($taskId: String!) {
        toggle(id: $taskId) {
          id
          title
          completed
        }
      }"""

  case class Variables(taskId: String)
  object Variables { implicit val jsonEncoder: Encoder[Variables] = deriveEncoder[Variables] }

  case class Data(toggle: Option[Task])
  object Data { implicit val jsonDecoder: Decoder[Data] = deriveDecoder[Data] }

  implicit val varEncoder: Encoder[Variables] = Variables.jsonEncoder
  implicit val dataDecoder: Decoder[Data]     = Data.jsonDecoder
}
