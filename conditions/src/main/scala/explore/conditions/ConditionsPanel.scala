// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.conditions

import cats.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import gem.Observation
import react.semanticui.collections.form.Form
import react.semanticui.collections.form.FormGroup
import explore.model.enum.ImageQuality
import explore.model.enum.CloudCover
import explore.model.enum.WaterVapor
import explore.model.enum.SkyBackground
import react.semanticui.widths._
import explore.model.Conditions
import explore.components.forms.EnumSelect
import explore.components.graphql.SubscriptionRenderMod
import explore.model.AppStateIO._
import clue.GraphQLQuery
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }
import cats.Show
import gem.util.Enumerated
import io.circe.HCursor
import io.circe.DecodingFailure
import io.circe.JsonObject
import crystal.react.io.implicits._
import monocle.macros.Lenses
import monocle.function.Cons.headOption
import crystal.react.StreamRendererMod.ModState
import cats.effect.IO
import monocle.Lens
import react.semanticui.elements.button.Button
import explore.components.undo.Undoer

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

final case class ConditionsPanel(
  observationId: Observation.Id
) extends ReactProps {
  @inline override def render: VdomElement = ConditionsPanel.component(this)
}

object ConditionsPanel {
  type Props = ConditionsPanel

  implicit def enumDecoder[E: Enumerated]: Decoder[E] = new Decoder[E] {
    final def apply(c: HCursor): Decoder.Result[E] =
      // TODO Obtain the failure CursorOp list from c.
      c.as[String]
        .flatMap(s =>
          Enumerated[E]
            .fromTag(s)
            .toRight(DecodingFailure(s"Invalid Enumerated value [$s] on [$c].", List.empty))
        )
  }
  implicit val conditionsDecoder = new Decoder[Conditions] {
    final def apply(c: HCursor): Decoder.Result[Conditions] =
      for {
        cc <- c.downField("cloud_cover").as[CloudCover]
        iq <- c.downField("image_quality").as[ImageQuality]
        sb <- c.downField("sky_background").as[SkyBackground]
        wv <- c.downField("water_vapor").as[WaterVapor]
      } yield {
        Conditions(cc, iq, sb, wv)
      }
  }

  implicit val propsReuse: Reusability[Props] = Reusability.by(_.observationId.format)

  private object Subscription extends GraphQLQuery {
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

  private object Mutation extends GraphQLQuery {
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

  implicit val showSkyBackground: Show[SkyBackground] =
    Show.show(_.label)

  implicit val showWaterVapor: Show[WaterVapor] =
    Show.show(_.label)

  implicit val showCloudCover: Show[CloudCover] =
    Show.show(_.label)

  implicit val showImageQuality: Show[ImageQuality] =
    Show.show(_.label)

  private def mutate(observationId: Observation.Id, fields: Mutation.Fields): IO[Unit] =
    AppState.clients.conditions
      .query(Mutation)(Mutation.Variables(observationId.format, fields).some)
      .as(())

  case class Modify(
    observationId: Observation.Id,
    conditions:    Conditions,
    modState:      ModState[Conditions],
    set:           Undoer.Set[Conditions]
  ) {
    def apply[A](
      lens:   Lens[Conditions, A],
      fields: A => Mutation.Fields
    )(
      value: A
    ): IO[Unit] =
      set(
        conditions,
        lens.asGetter, { v: A =>
          for {
            _ <- IO(println(s"MODIFY! [${fields(v)}]"))
            _ <- modState(lens.set(v)).toIO // TODO Change modState in crystal to IO? (instead of Callback)
            _ <- mutate(observationId, fields(v))
          } yield ()
        }
      )(value)
  }

  private def someEnumTag[E: Enumerated](e: E): Option[String] =
    Enumerated[E].tag(e).some

  private def iqFields(iq: ImageQuality): Mutation.Fields =
    Mutation.Fields(image_quality = someEnumTag(iq))

  private def ccFields(cc: CloudCover): Mutation.Fields =
    Mutation.Fields(cloud_cover = someEnumTag(cc))

  private def wvFields(wv: WaterVapor): Mutation.Fields =
    Mutation.Fields(water_vapor = someEnumTag(wv))

  private def sbFields(sb: SkyBackground): Mutation.Fields =
    Mutation.Fields(sky_background = someEnumTag(sb))

  protected val component =
    ScalaComponent
      .builder[ConditionsPanel]("ConditionsPanel")
      .render { $ =>
        SubscriptionRenderMod[Subscription.Data, Conditions](
          AppState.clients.conditions
            .subscribe(Subscription)(
              Subscription.Variables($.props.observationId.format).some
            ),
          _.map(Subscription.Data.conditions.composeOptional(headOption).getOption _).unNone
        ) { (conditions, modState) =>
          Undoer {
            (
              set:  Undoer.Set[Conditions],
              undo: Undoer.Undo[Conditions],
              redo: Undoer.Redo[Conditions]
            ) =>
              val modify = Modify($.props.observationId, conditions, modState, set)

              <.div(
                Form(
                  FormGroup(widths = Two)(
                    EnumSelect[ImageQuality]("Image Quality",
                                             conditions.iq.some,
                                             "Select",
                                             disabled = false,
                                             modify(Conditions.iq, iqFields)),
                    EnumSelect[CloudCover]("Cloud Cover",
                                           conditions.cc.some,
                                           "Select",
                                           disabled = false,
                                           modify(Conditions.cc, ccFields))
                  ),
                  FormGroup(widths = Two)(
                    EnumSelect[WaterVapor]("Water Vapor",
                                           conditions.wv.some,
                                           "Select",
                                           disabled = false,
                                           modify(Conditions.wv, wvFields)),
                    EnumSelect[SkyBackground]("Sky Background",
                                              conditions.sb.some,
                                              "Select",
                                              disabled = false,
                                              modify(Conditions.sb, sbFields))
                  )
                ),
                Button(onClick = undo(conditions))("Undo"),
                Button(onClick = redo(conditions))("Redo")
              )
          }
        }
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
