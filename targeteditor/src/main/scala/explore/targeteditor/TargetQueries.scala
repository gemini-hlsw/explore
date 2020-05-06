// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.target

import cats.effect.IO
import cats.implicits._
import crystal.react._
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }
import explore.implicits._
import explore.undo.Undoer
import gem.Observation
import gsp.math.Coordinates
import gsp.math.Declination
import gsp.math.Epoch
import gsp.math.ProperMotion
import gsp.math.RightAscension
import io.circe.Decoder
import io.circe.Encoder
import io.circe.HCursor
import io.circe.JsonObject
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder
import monocle.macros.Lenses
import clue.GraphQLQuery
import explore.model.SiderealTarget

object TargetQueries {

  implicit val targetDecoder = new Decoder[SiderealTarget] {
    final def apply(c: HCursor): Decoder.Result[SiderealTarget] =
      for {
        name <- c.downField("name").as[String]
        ra   <- c.downField("ra").as[String].map(RightAscension.fromStringHMS.getOption)
        dec  <- c.downField("dec").as[String].map(Declination.fromStringSignedDMS.getOption)
      } yield {
        val coords =
          ProperMotion((ra, dec).mapN(Coordinates.apply).getOrElse(Coordinates.Zero),
                       Epoch.J2000,
                       none,
                       none,
                       none
          )
        SiderealTarget(name, coords)
      }
  }

  object Subscription extends GraphQLQuery {
    val document = """
      subscription ($observationId: String!) {
        targets(where: {observation_id: {_eq: $observationId}}) {
          name
          object_type
          ra
          dec
        }
      }
      """

    case class Variables(observationId: String)
    object Variables { implicit val jsonEncoder: Encoder[Variables] = deriveEncoder[Variables] }

    @Lenses
    case class Data(targets: List[SiderealTarget])
    object Data { implicit val jsonDecoder: Decoder[Data] = deriveDecoder[Data] }

    implicit val varEncoder: Encoder[Variables] = Variables.jsonEncoder
    implicit val dataDecoder: Decoder[Data]     = Data.jsonDecoder
  }

  object Mutation extends GraphQLQuery {
    val document = """
      mutation ($observationId: String, $fields: targets_set_input){
        update_targets(_set: $fields, where: {
          observation_id: {
            _eq: $observationId
          }
        }) {
          affected_rows
        }
      }
    """

    case class Fields(
      name: Option[String] = None,
      ra:   Option[String] = None,
      dec:  Option[String] = None
    )
    object Fields    {
      implicit val jsonEncoder: Encoder[Fields] = deriveEncoder[Fields].mapJson(_.dropNullValues)
    }

    case class Variables(observationId: String, fields: Fields)
    object Variables { implicit val jsonEncoder: Encoder[Variables] = deriveEncoder[Variables] }

    case class Data(update_conditions: JsonObject) // We are ignoring affected_rows
    object Data { implicit val jsonDecoder: Decoder[Data] = deriveDecoder[Data] }

    implicit val varEncoder: Encoder[Variables] = Variables.jsonEncoder
    implicit val dataDecoder: Decoder[Data]     = Data.jsonDecoder
  }

  private def mutate(observationId: Observation.Id, fields: Mutation.Fields)(implicit
    ctx: AppContextIO
  ): IO[Unit] =
    ctx.clients.conditions
      .query(Mutation)(Mutation.Variables(observationId.format, fields).some)
      .as(())

  case class Modify(
    observationId: Observation.Id,
    target:        SiderealTarget,
    modState:      ModState[IO, SiderealTarget],
    setter:        Undoer.Setter[IO, SiderealTarget]
  )(implicit ctx:  AppContextIO) {
    def apply[A](
      get:    SiderealTarget => A,
      set:    A => SiderealTarget => SiderealTarget,
      fields: A => Mutation.Fields
    )(
      value:  A
    ): IO[Unit] =
      setter.set(
        target,
        get,
        { value: A =>
          for {
            _ <- (modState.apply _).compose(set)(value)
            _ <- mutate(observationId, fields(value))
          } yield ()
        }
      )(value)
  }

}
