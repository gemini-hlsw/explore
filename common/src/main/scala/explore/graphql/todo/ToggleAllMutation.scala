package explore.graphql

import client.GraphQLQuery
import explore.model.Task
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }

object ToggleAllMutation extends GraphQLQuery {
  val document = """
      mutation ToggleAllMutation($$checked: Boolean!) {
        toggleAll(checked: $$checked) {
          id
          title
          completed
        }
      }"""

  case class Variables(checked: Boolean)
  object Variables { implicit val jsonEncoder: Encoder[Variables] = deriveEncoder[Variables] }

  case class Data(toggleAll: Option[List[Task]])
  object Data { implicit val jsonDecoder: Decoder[Data] = deriveDecoder[Data] }

  implicit val varEncoder: Encoder[Variables] = Variables.jsonEncoder
  implicit val dataDecoder: Decoder[Data]     = Data.jsonDecoder
}
