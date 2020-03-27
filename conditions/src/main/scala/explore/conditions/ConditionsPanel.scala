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
import explore.components.graphql.SubscriptionRender
import explore.model.AppStateIO._
import clue.GraphQLQuery
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }
import cats.Show
import gem.util.Enumerated
import io.circe.HCursor
import io.circe.DecodingFailure

/*
query {
  conditions {
    observation_id
  }
}

subscription MyQuery {
  conditions(where: {observation_id: {_eq: "368e5b67-6c1e-4d77-8547-ef16766802fd"}}) {
    observation_id
    cloud_cover
    image_quality
    sky_background
    water_vapor
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

mutation {
  update_conditions(_set: {
    cloud_cover: "Percent50",
    image_quality: "Any",
    sky_background: "Any",
    water_vapor:"Any"
  }, where: {
    observation_id: {
      _eq: "368e5b67-6c1e-4d77-8547-ef16766802fd"
    }
  }) {
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

  implicit def enumDecoder[E : Enumerated]: Decoder[E] = new Decoder[E] {
    final def apply(c: HCursor): Decoder.Result[E] =
      // TODO Obtain the failure CursorOp list from c.
      c.as[String].flatMap(s => Enumerated[E].fromTag(s).toRight(DecodingFailure(s"Invalid Enumerated value [$s] on [$c].", List.empty)))
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

    case class Data(conditions: List[Conditions])
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

  private def iqChanged(iq: ImageQuality) = Callback(println(iq))
  private def ccChanged(cc: CloudCover) = Callback(println(cc))
  private def wvChanged(wv: WaterVapor) = Callback(println(wv))
  private def sbChanged(sb: SkyBackground) = Callback(println(sb))

  protected val component =
    ScalaComponent
      .builder[ConditionsPanel]("ConditionsPanel")
      .render { $ =>
        SubscriptionRender[Subscription.Data](
          AppState.clients.conditions
            .subscribe(Subscription)(
              Subscription.Variables($.props.observationId.format).some
            )
        )(
          data =>
            <.div(
              data.conditions.headOption.whenDefined( conditions =>
                Form(
                  FormGroup(widths = Two)(
                    EnumSelect[ImageQuality]("Image Quality",
                                            conditions.iq.some,
                                            "Select",
                                            disabled = false,
                                            iqChanged),
                    EnumSelect[CloudCover]("Cloud Cover",
                                          conditions.cc.some,
                                          "Select",
                                          disabled = false,
                                          ccChanged)
                  ),
                  FormGroup(widths = Two)(
                    EnumSelect[WaterVapor]("Water Vapor",
                                          conditions.wv.some,
                                          "Select",
                                          disabled = false,
                                          wvChanged),
                    EnumSelect[SkyBackground]("Sky Background",
                                              conditions.sb.some,
                                              "Select",
                                              disabled = false,
                                              sbChanged)
                  )            
                )
              )
            )
        )
      }
      .build
}
