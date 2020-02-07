package explore.graphql

import clue.GraphQLQuery
import explore.model.Task
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }

object SaveMutation extends GraphQLQuery {
  val document = """
  mutation SaveMutation($taskId: String!, $title: String!) {
    save(id: $taskId, title: $title) {
      id
      title
      completed
    }
  }"""

  case class Variables(taskId: String, title: String)
  object Variables { implicit val jsonEncoder: Encoder[Variables] = deriveEncoder[Variables] }

  case class Data(save: Option[Task])
  object Data { implicit val jsonDecoder: Decoder[Data] = deriveDecoder[Data] }

  implicit val varEncoder: Encoder[Variables] = Variables.jsonEncoder
  implicit val dataDecoder: Decoder[Data]     = Data.jsonDecoder
}
