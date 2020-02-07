package explore.graphql.polls

import clue.GraphQLQuery
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }

object OnlineUsersCountSubscription extends GraphQLQuery {
  val document = """"
    subscription OnlineUsersCountSubscription {
      online_users {
        count
      }
    }
  """

  case class Variables()
  object Variables { implicit val jsonEncoder: Encoder[Variables] = deriveEncoder[Variables] }

  case class Data(online_users: List[Online_users])
  object Data { implicit val jsonDecoder: Decoder[Data] = deriveDecoder[Data] }

  case class Online_users(count: Option[Long])
  object Online_users {
    implicit val jsonDecoder: Decoder[Online_users] = deriveDecoder[Online_users]
    implicit val jsonEncoder: Encoder[Online_users] = deriveEncoder[Online_users]
  }

  implicit val varEncoder: Encoder[Variables] = Variables.jsonEncoder
  implicit val dataDecoder: Decoder[Data]     = Data.jsonDecoder
}
