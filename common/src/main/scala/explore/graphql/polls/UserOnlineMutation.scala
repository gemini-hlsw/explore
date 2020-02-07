package explore.graphql.polls

import clue.GraphQLQuery
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }
import java.util.UUID

object UserOnlineMutation extends GraphQLQuery {
  val document = """
    mutation UserOnlineMutation($uuid: uuid) {
      update_user(where: {id: {_eq: $uuid}}, _set: {online_ping: true}) {
        affected_rows
        returning {
          last_seen_at
        }
      }
    }
  """

  case class Variables(uuid: Option[UUID])
  object Variables { implicit val jsonEncoder: Encoder[Variables] = deriveEncoder[Variables] }

  case class Data(update_user: Option[Update_user])
  object Data { implicit val jsonDecoder: Decoder[Data] = deriveDecoder[Data] }

  case class Update_user(affected_rows: Int, returning: List[Update_user.Returning])
  object Update_user {
    implicit val jsonDecoder: Decoder[Update_user] = deriveDecoder[Update_user]
    implicit val jsonEncoder: Encoder[Update_user] = deriveEncoder[Update_user]

    case class Returning(last_seen_at: Option[String]) // timestamptz - Use ZonedDateTime. Need custom [De|En]coder.
    object Returning {
      implicit val jsonDecoder: Decoder[Returning] = deriveDecoder[Returning]
      implicit val jsonEncoder: Encoder[Returning] = deriveEncoder[Returning]
    }
  }

  implicit val varEncoder: Encoder[Variables] = Variables.jsonEncoder
  implicit val dataDecoder: Decoder[Data]     = Data.jsonDecoder
}
