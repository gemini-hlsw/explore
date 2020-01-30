package explore.graphql

import client.GraphQLQuery
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }

object TestQuery extends GraphQLQuery {
  val document = """
        query PersonsTest {    
            allPersons {
                name
                films {
                    director
                }
            }
        }"""

  case class Variables()
  object Variables { implicit val jsonEncoder: Encoder[Variables] = deriveEncoder[Variables] }

  case class Data(allPersons: List[AllPersons])
  object Data { implicit val jsonDecoder: Decoder[Data] = deriveDecoder[Data] }

  case class AllPersons(name: String, films: Option[List[AllPersons.Films]])
  object AllPersons {
    implicit val jsonDecoder: Decoder[AllPersons] = deriveDecoder[AllPersons]
    implicit val jsonEncoder: Encoder[AllPersons] = deriveEncoder[AllPersons]

    case class Films(director: Option[String])
    object Films {
      implicit val jsonDecoder: Decoder[Films] = deriveDecoder[Films]
      implicit val jsonEncoder: Encoder[Films] = deriveEncoder[Films]
    }
  }

  implicit val varEncoder: Encoder[Variables] = Variables.jsonEncoder
  implicit val dataDecoder: Decoder[Data]     = Data.jsonDecoder
}
