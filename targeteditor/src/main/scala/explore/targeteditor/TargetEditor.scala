// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.target

import cats.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
// import crystal.react.implicits._
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }
import explore._
import explore.implicits._
// import explore.model._
import monocle.macros.Lenses
import monocle.function.Cons.headOption
import clue.GraphQLQuery
import gem.Observation
import gem.Target
import io.circe.HCursor
import gsp.math.RightAscension
import gsp.math.Declination
import gsp.math.ProperMotion
import gsp.math.Coordinates
import gsp.math.Epoch
import explore.components.graphql.SubscriptionRenderMod

final case class TargetEditor(
  observationId:    Observation.Id
)(implicit val ctx: AppContextIO)
    extends ReactProps {
  @inline override def render: VdomElement = TargetEditor.component(this)
  // val searchTerm                          = target.zoomL(ExploreTarget.searchTermL).get
}

object TargetEditor    {
  type Props = TargetEditor

  implicit val targetDecoder = new Decoder[Target] {
    final def apply(c: HCursor): Decoder.Result[Target] =
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
        Target(name, coords.asRight)
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
    case class Data(targets: List[Target])
    object Data { implicit val jsonDecoder: Decoder[Data] = deriveDecoder[Data] }

    implicit val varEncoder: Encoder[Variables] = Variables.jsonEncoder
    implicit val dataDecoder: Decoder[Data]     = Data.jsonDecoder
  }

  val component              =
    ScalaComponent
      .builder[Props]("TargetEditor")
      .render_P { props =>
        implicit val appCtx = props.ctx
        SubscriptionRenderMod[Subscription.Data, Target](
          appCtx.clients.conditions
            .subscribe(Subscription)(
              Subscription.Variables(props.observationId.format).some
            ),
          _.map(Subscription.Data.targets.composeOptional(headOption).getOption _).unNone
        )(view => TargetBody(view))
      }
      .build

}
