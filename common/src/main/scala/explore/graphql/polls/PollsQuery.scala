package explore.graphql.polls

import explore.graphql.client.GraphQLQuery
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }
import explore.model.Poll

object PollsQuery extends GraphQLQuery {
  val document = """
    query PollsQuery {
      poll {
        id
        question
        options(order_by: {id: desc}) {
          id
          text
        }
      }
    }
  """

  case class Variables()
  object Variables { implicit val jsonEncoder: Encoder[Variables] = deriveEncoder[Variables] }

  case class Data(poll: List[Poll])
  object Data { implicit val jsonDecoder: Decoder[Data] = deriveDecoder[Data] }

  implicit val varEncoder: Encoder[Variables] = Variables.jsonEncoder
  implicit val dataDecoder: Decoder[Data]     = Data.jsonDecoder
}
