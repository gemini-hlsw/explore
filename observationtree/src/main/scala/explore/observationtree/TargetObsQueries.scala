// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.implicits._
import clue.GraphQLQuery
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
import monocle.function.Cons.headOption
import monocle.macros.Lenses
import monocle.Lens
import crystal.ViewF
import cats.effect.ContextShift
import explore.model.decoders._
import explore.model.encoders._
import explore.model.SiderealTarget
import explore.model.ExploreObservation
import gsp.math.RightAscension
import gsp.math.Declination
import gsp.math.ProperMotion
import gsp.math.Epoch
import org.locationtech.jts.geom.Coordinate
import gsp.math.Coordinates
import explore.model.enum.ObsStatus
import java.time.Duration
import io.circe.Json
import cats.Traverse
import japgolly.scalajs.react.Reusability
import java.util.UUID
import cats.effect.Effect

object TargetObsQueries {

  case class TargetWithObs(target: SiderealTarget, obs: List[ExploreObservation])

  @Lenses
  case class TargetsWithObs(targets: List[SiderealTarget], obs: List[ExploreObservation])

  implicit val targetWithObsDecoder = new Decoder[TargetWithObs] {
    final def apply(c: HCursor): Decoder.Result[TargetWithObs] =
      for {
        target  <- c.as[SiderealTarget]
        obsJson <- c.downField("observations").as[List[Json]].map(_.map(_.hcursor))
        obsList <- Traverse[List].traverse(obsJson)(obsDecoder(target).apply)
      } yield TargetWithObs(target, obsList)
  }

  implicit val targetsWithObsDecoder = new Decoder[TargetsWithObs] {
    final def apply(c: HCursor): Decoder.Result[TargetsWithObs] =
      c.as[List[TargetWithObs]].map { targetsWithObs =>
        TargetsWithObs(targetsWithObs.map(_.target), targetsWithObs.flatMap(_.obs))
      }
  }

  object Subscription extends GraphQLQuery {
    val document = """
      subscription {
        targets {
          id
          name
          ra
          dec
          observations {
            id
            status
            configuration
            constraint {
              name
            }
            duration_seconds
          }
        }
      }
    """

    case class Variables()
    object Variables { implicit val jsonEncoder: Encoder[Variables] = deriveEncoder[Variables] }

    @Lenses
    case class Data(targets: TargetsWithObs)
    object Data { implicit val jsonDecoder: Decoder[Data] = deriveDecoder[Data] }

    implicit val varEncoder: Encoder[Variables] = Variables.jsonEncoder
    implicit val dataDecoder: Decoder[Data]     = Data.jsonDecoder
  }

  object ObsMutation extends GraphQLQuery {
    val document = """
      mutation ($id: uuid, $fields: observations_set_input){
        update_observations(_set: $fields, where: {id: {_eq: $id}}) {
          affected_rows
        }
      }
    """

    case class Fields(
      target_id: Option[SiderealTarget.Id] = None
    )
    object Fields {
      implicit val jsonEncoder: Encoder[Fields] = deriveEncoder[Fields].mapJson(_.dropNullValues)
    }

    case class Variables(id: ExploreObservation.Id, fields: Fields)
    object Variables { implicit val jsonEncoder: Encoder[Variables] = deriveEncoder[Variables] }

    case class Data(update_observations: JsonObject) // We are ignoring affected_rows
    object Data { implicit val jsonDecoder: Decoder[Data] = deriveDecoder[Data] }

    implicit val varEncoder: Encoder[Variables] = Variables.jsonEncoder
    implicit val dataDecoder: Decoder[Data]     = Data.jsonDecoder
  }

  object NewTarget extends GraphQLQuery {
    val document = """
      mutation($target: targets_insert_input!) {
        insert_targets_one(object: $target) {
          id
        }
      }
    """

    case class Variables(target: SiderealTarget)
    object Variables { implicit val jsonEncoder: Encoder[Variables] = deriveEncoder[Variables] }

    case class Data(insert_targets_one: SiderealTarget.Id)
    object Data { implicit val jsonDecoder: Decoder[Data] = deriveDecoder[Data] }

    implicit val varEncoder: Encoder[Variables] = Variables.jsonEncoder
    implicit val dataDecoder: Decoder[Data]     = Data.jsonDecoder
  }

  def mutateObs(obsId: ExploreObservation.Id, fields: ObsMutation.Fields): IO[Unit] =
    AppCtx.flatMap(
      _.clients.programs
        .query(ObsMutation)(ObsMutation.Variables(obsId, fields).some)
        .void
    )

  def createTarget(onError: Throwable => IO[Unit]): IO[SiderealTarget.Id] =
    SiderealTarget
      .New[IO]
      .flatMap(target =>
        Effect[IO]
          .runAsync(
            AppCtx.flatMap(
              _.clients.programs
                .query(NewTarget)(NewTarget.Variables(target).some)
                .map(_.insert_targets_one)
            )
          ) {
            case Left(t) => onError(t)
            case _       => IO.unit
          }
          .toIO
          .map(_ => target.id)
      )

  implicit val targetsWithObsReusability: Reusability[TargetsWithObs] = Reusability.derive

  def targetObsSubscription(
    render: View[TargetsWithObs] => VdomNode
  ): SubscriptionRenderMod[Subscription.Data, TargetsWithObs] =
    AppCtx.withCtx { implicit appCtx =>
      SubscriptionRenderMod[Subscription.Data, TargetsWithObs](
        appCtx.clients.programs
          .subscribe(Subscription)(),
        _.map(
          Subscription.Data.targets.get
        )
      )(render)
    }
}
