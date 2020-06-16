// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.conditions

import cats.effect.IO
import cats.implicits._
import clue.GraphQLQuery
import crystal.react.ModState
import explore.AppCtx
import explore.components.graphql.SubscriptionRenderMod
import explore.implicits._
import explore.model.Conditions
import explore.model.enum.CloudCover
import explore.model.enum.ImageQuality
import explore.model.enum.SkyBackground
import explore.model.enum.WaterVapor
import explore.model.reusability._
import explore.undo.Undoer
import gem.Observation
import gem.util.Enumerated
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.JsonObject
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import monocle.function.Cons.headOption
import monocle.macros.Lenses

object ConditionsQueries {
  /*
query {
  conditions {
    observation_id
  }
}

mutation {
  insert_conditions(objects: [{
    observation_id: "368e5b67-6c1e-4d77-8547-ef16766802fe",
    cloud_cover: "Any",
    image_quality: "Any",
    sky_background: "Any",
    water_vapor: "Any"
  }]) {
    affected_rows
  }
}
   */

  implicit def enumDecoder[E: Enumerated]: Decoder[E] =
    new Decoder[E] {
      final def apply(c: HCursor): Decoder.Result[E] =
        // TODO Obtain the failure CursorOp list from c.
        c.as[String]
          .flatMap(s =>
            Enumerated[E]
              .fromTag(s)
              .toRight(DecodingFailure(s"Invalid Enumerated value [$s] on [$c].", List.empty))
          )
    }
  implicit val conditionsDecoder                      = new Decoder[Conditions] {
    final def apply(c: HCursor): Decoder.Result[Conditions] =
      for {
        cc <- c.downField("cloud_cover").as[CloudCover]
        iq <- c.downField("image_quality").as[ImageQuality]
        sb <- c.downField("sky_background").as[SkyBackground]
        wv <- c.downField("water_vapor").as[WaterVapor]
      } yield Conditions(cc, iq, sb, wv)
  }

  object Subscription extends GraphQLQuery {
    val document = """
      subscription ($observationId: String!) {
        conditions(where: {observation_id: {_eq: $observationId}}) {
          cloud_cover
          image_quality
          sky_background
          water_vapor
        }
      }
      """

    case class Variables(observationId: String)
    object Variables { implicit val jsonEncoder: Encoder[Variables] = deriveEncoder[Variables] }

    @Lenses
    case class Data(conditions: List[Conditions])
    object Data { implicit val jsonDecoder: Decoder[Data] = deriveDecoder[Data] }

    implicit val varEncoder: Encoder[Variables] = Variables.jsonEncoder
    implicit val dataDecoder: Decoder[Data]     = Data.jsonDecoder
  }

  object Mutation extends GraphQLQuery {
    val document = """
      mutation ($observationId: String, $fields: conditions_set_input){
        update_conditions(_set: $fields, where: {
          observation_id: {
            _eq: $observationId
          }
        }) {
          affected_rows
        }
      }
    """

    case class Fields(
      cloud_cover:    Option[String] = None,
      image_quality:  Option[String] = None,
      sky_background: Option[String] = None,
      water_vapor:    Option[String] = None
    )
    object Fields {
      implicit val jsonEncoder: Encoder[Fields] = deriveEncoder[Fields].mapJson(_.dropNullValues)
    }

    case class Variables(observationId: String, fields: Fields)
    object Variables { implicit val jsonEncoder: Encoder[Variables] = deriveEncoder[Variables] }

    case class Data(update_conditions: JsonObject) // We are ignoring affected_rows
    object Data { implicit val jsonDecoder: Decoder[Data] = deriveDecoder[Data] }

    implicit val varEncoder: Encoder[Variables] = Variables.jsonEncoder
    implicit val dataDecoder: Decoder[Data]     = Data.jsonDecoder
  }

  case class Modify(
    observationId: Observation.Id,
    conditions:    Conditions,
    modState:      ModState[IO, Conditions],
    setter:        Undoer.Setter[IO, Conditions]
  ) {
    def apply[A](
      get:    Conditions => A,
      set:    A => Conditions => Conditions,
      fields: A => Mutation.Fields
    ): A => IO[Unit] =
      setter.set(
        conditions,
        get,
        { value: A =>
          for {
            _ <- (modState.apply _).compose(set)(value)
            _ <- mutate(observationId, fields(value))
          } yield ()
        }
      )
  }

  def someEnumTag[E: Enumerated](e: E): Option[String] =
    Enumerated[E].tag(e).some

  def iqFields(iq: ImageQuality): Mutation.Fields =
    Mutation.Fields(image_quality = someEnumTag(iq))

  def ccFields(cc: CloudCover): Mutation.Fields =
    Mutation.Fields(cloud_cover = someEnumTag(cc))

  def wvFields(wv: WaterVapor): Mutation.Fields =
    Mutation.Fields(water_vapor = someEnumTag(wv))

  def sbFields(sb: SkyBackground): Mutation.Fields =
    Mutation.Fields(sky_background = someEnumTag(sb))

  private def mutate(observationId: Observation.Id, fields: Mutation.Fields): IO[Unit] =
    AppCtx.flatMap(
      _.clients.programs
        .query(Mutation)(Mutation.Variables(observationId.format, fields).some)
        .void
    )

  def conditionsSubscription(
    obsId:  Observation.Id
  )(render: View[Conditions] => VdomNode): SubscriptionRenderMod[Subscription.Data, Conditions] =
    AppCtx.withCtx { implicit appCtx =>
      SubscriptionRenderMod[Subscription.Data, Conditions](
        appCtx.clients.programs
          .subscribe(Subscription)(
            Subscription.Variables(obsId.format).some
          ),
        _.map(
          Subscription.Data.conditions.composeOptional(headOption).getOption _
        ).unNone
      )(render)
    }
}
