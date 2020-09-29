// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import cats.effect.ContextShift
import cats.effect.IO
import cats.syntax.all._
import clue.GraphQLQuery
import crystal.ViewF
import explore.AppCtx
import explore.components.graphql.SubscriptionRenderMod
import explore.implicits._
import explore.model.Constraints
import explore.model.decoders._
import explore.model.enum.CloudCover
import explore.model.enum.ImageQuality
import explore.model.enum.SkyBackground
import explore.model.enum.WaterVapor
import explore.model.reusability._
import explore.undo.Undoer
import io.circe.Decoder
import io.circe.Encoder
import io.circe.JsonObject
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.util.Enumerated
import monocle.Lens
import monocle.function.Cons.headOption
import monocle.macros.Lenses

object ConstraintsQueries {

  object Subscription extends GraphQLQuery {
    val document = """
      subscription ($id: uuid!) {
        constraints(where: {id: {_eq: $id}}) {
          id
          name
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

  def ConstraintsSubscription(
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
