package explore.graphql.polls

import clue.GraphQLQuery
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }
import java.util.UUID

object VoteMutation extends GraphQLQuery {
  val document = """
    mutation VoteMutation($optionId: uuid!, $userId: uuid!) {
      insert_vote(objects: [{option_id: $optionId, created_by_user_id: $userId}]) {
        returning {
          id
        }
      }
    }
  """

  case class Variables(optionId: UUID, userId: UUID)
  object Variables { implicit val jsonEncoder: Encoder[Variables] = deriveEncoder[Variables] }

  case class Data(insert_vote: Option[Insert_vote])
  object Data { implicit val jsonDecoder: Decoder[Data] = deriveDecoder[Data] }

  case class Insert_vote(returning: List[Insert_vote.Returning])
  object Insert_vote {
    implicit val jsonDecoder: Decoder[Insert_vote] = deriveDecoder[Insert_vote]
    implicit val jsonEncoder: Encoder[Insert_vote] = deriveEncoder[Insert_vote]

    case class Returning(id: UUID)
    object Returning {
      implicit val jsonDecoder: Decoder[Returning] = deriveDecoder[Returning]
      implicit val jsonEncoder: Encoder[Returning] = deriveEncoder[Returning]
    }
  }

  implicit val varEncoder: Encoder[Variables] = Variables.jsonEncoder
  implicit val dataDecoder: Decoder[Data]     = Data.jsonDecoder
}
