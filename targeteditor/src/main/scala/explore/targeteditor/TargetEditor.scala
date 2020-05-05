// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.target

import cats.effect.IO
import cats.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import crystal.react._
import io.circe.{ Decoder, Encoder }
import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }
import explore.implicits._
import explore.components.graphql.SubscriptionRenderMod
import explore.undo.Undoer
import gem.Observation
import gem.Target
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
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import monocle.function.Cons.headOption
import monocle.macros.Lenses
import react.common._
import clue.GraphQLQuery

final case class TargetEditor(
  observationId:    CtxIO[Observation.Id]
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
    target:        Target,
    setState:      SetState[IO, Target],
    setter:        Undoer.Setter[IO, Target]
  )(implicit ctx:  AppContextIO) {
    def apply[A](
      lens:   Lens[Target, A],
      fields: A => Mutation.Fields
    )(
      value:  A
    ): IO[Unit] =
      setter.set(
        target,
        lens,
        { c: Target =>
          for {
            _ <- setState(c)
            _ <- mutate(observationId, fields(lens.get(c)))
          } yield ()
        }
      )(value)
  }
  val component =
    ScalaComponent
      .builder[Props]("TargetEditor")
      .render_P { props =>
        implicit val appCtx = props.ctx
        SubscriptionRenderMod[Subscription.Data, Target](
          appCtx.clients.conditions
            .subscribe(Subscription)(
              Subscription.Variables(props.observationId.value.format).some
            ),
          _.map(Subscription.Data.targets.composeOptional(headOption).getOption _).unNone
        ) { view =>
          TargetBody(props.observationId.value, view)
        }
      }
      .build

}
