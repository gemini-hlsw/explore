package explore.graphql.polls

import clue.GraphQLQuery
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }
import java.util.UUID

object NewUserMutation extends GraphQLQuery {
  val document = """
    mutation NewUserMutation($uuid: uuid) {
      insert_user(objects: [{id: $uuid}]) {
        returning {
          id
          created_at
        }
      }
    }
  """

  case class Variables(uuid: Option[UUID])
  object Variables { implicit val jsonEncoder: Encoder[Variables] = deriveEncoder[Variables] }

  case class Data(insert_user: Option[Insert_user])
  object Data { implicit val jsonDecoder: Decoder[Data] = deriveDecoder[Data] }

  case class Insert_user(returning: List[Insert_user.Returning])
  object Insert_user {
    implicit val jsonDecoder: Decoder[Insert_user] = deriveDecoder[Insert_user]
    implicit val jsonEncoder: Encoder[Insert_user] = deriveEncoder[Insert_user]

    case class Returning(id: UUID, created_at: String) //  // timestamptz - Use ZonedDateTime. Need custom [De|En]coder.
    object Returning {
      implicit val jsonDecoder: Decoder[Returning] = deriveDecoder[Returning]
      implicit val jsonEncoder: Encoder[Returning] = deriveEncoder[Returning]
    }
  }

  implicit val varEncoder: Encoder[Variables] = Variables.jsonEncoder
  implicit val dataDecoder: Decoder[Data]     = Data.jsonDecoder
}
