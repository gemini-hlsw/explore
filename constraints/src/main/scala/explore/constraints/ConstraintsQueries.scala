// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import cats.effect.ContextShift
import cats.effect.IO
import cats.implicits._
import clue.GraphQLQuery
import crystal.ViewF
import crystal.react.ModState
import explore.AppCtx
import explore.components.graphql.SubscriptionRenderMod
import explore.implicits._
import explore.model.Constraints
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
import monocle.Lens
import monocle.function.Cons.headOption
import monocle.macros.Lenses

object ConstraintsQueries {
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
  implicit val constraintsDecoder                     = new Decoder[Constraints] {
    final def apply(c: HCursor): Decoder.Result[Constraints] =
      for {
        cc <- c.downField("cloud_cover").as[CloudCover]
        iq <- c.downField("image_quality").as[ImageQuality]
        sb <- c.downField("sky_background").as[SkyBackground]
        wv <- c.downField("water_vapor").as[WaterVapor]
      } yield Constraints(cc, iq, sb, wv)
  }

  object Subscription extends GraphQLQuery {
    val document = """
      subscription ($id: uuid!) {
        constraints(where: {id: {_eq: $id}}) {
          cloud_cover
          image_quality
          sky_background
          water_vapor
        }
      }
      """
    case class Variables(id: Constraints.Id)

    object Variables { implicit val jsonEncoder: Encoder[Variables] = deriveEncoder[Variables] }

    @Lenses
    case class Data(constraints: List[Constraints])
    object Data { implicit val jsonDecoder: Decoder[Data] = deriveDecoder[Data] }

    implicit val varEncoder: Encoder[Variables] = Variables.jsonEncoder
    implicit val dataDecoder: Decoder[Data]     = Data.jsonDecoder
  }

  object Mutation extends GraphQLQuery {
    val document = """
      mutation ($id: uuid, $fields: constraints_set_input){
        update_constraints(_set: $fields, where: {id: {_eq: $id}}) {
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

    case class Variables(id: Constraints.Id, fields: Fields)
    object Variables { implicit val jsonEncoder: Encoder[Variables] = deriveEncoder[Variables] }

    case class Data(update_constraints: JsonObject) // We are ignoring affected_rows
    object Data { implicit val jsonDecoder: Decoder[Data] = deriveDecoder[Data] }

    implicit val varEncoder: Encoder[Variables] = Variables.jsonEncoder
    implicit val dataDecoder: Decoder[Data]     = Data.jsonDecoder
  }

  case class UndoViewZoom(
    id:     Constraints.Id,
    view:   View[Constraints],
    setter: Undoer.Setter[IO, Constraints]
  ) {
    def apply[A](
      lens:        Lens[Constraints, A],
      fields:      A => Mutation.Fields
    )(implicit cs: ContextShift[IO]): View[A] =
      ViewF[IO, A](
        lens.get(view.get),
        setter.mod(
          view.get,
          lens.get,
          { value: A =>
            for {
              _ <- view.mod.compose(lens.set)(value)
              _ <- mutate(id, fields(value))
            } yield ()
          }
        )
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

  private def mutate(id: Constraints.Id, fields: Mutation.Fields): IO[Unit] =
    AppCtx.flatMap(
      _.clients.programs
        .query(Mutation)(Mutation.Variables(id, fields).some)
        .void
    )

  def constraintsSubscription(
    id:     Constraints.Id
  )(render: View[Constraints] => VdomNode): SubscriptionRenderMod[Subscription.Data, Constraints] =
    AppCtx.withCtx { implicit appCtx =>
      SubscriptionRenderMod[Subscription.Data, Constraints](
        appCtx.clients.programs
          .subscribe(Subscription)(
            Subscription.Variables(id).some
          ),
        _.map(
          Subscription.Data.constraints.composeOptional(headOption).getOption _
        ).unNone
      )(render)
    }
}
