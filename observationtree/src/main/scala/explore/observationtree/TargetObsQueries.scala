// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import java.time.Duration
import java.util.UUID

import cats.Traverse
import cats.effect.ContextShift
import cats.effect.Effect
import cats.effect.IO
import cats.implicits._
import clue.GraphQLQuery
import crystal.ViewF
import crystal.react.ModState
import explore.AppCtx
import explore.components.graphql.SubscriptionRenderMod
import explore.implicits._
import explore.model.Constraints
import explore.model.ExploreObservation
import explore.model.SiderealTarget
import explore.model.decoders._
import explore.model.encoders._
import explore.model.enum.CloudCover
import explore.model.enum.ImageQuality
import explore.model.enum.ObsStatus
import explore.model.enum.SkyBackground
import explore.model.enum.WaterVapor
import explore.model.reusability._
import explore.undo.Undoer
import gem.Observation
import gem.util.Enumerated
import gsp.math.Coordinates
import gsp.math.Declination
import gsp.math.Epoch
import gsp.math.ProperMotion
import gsp.math.RightAscension
import io.circe.Decoder
import io.circe.DecodingFailure
import io.circe.Encoder
import io.circe.HCursor
import io.circe.Json
import io.circe.JsonObject
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Lens
import monocle.function.Cons.headOption
import monocle.macros.Lenses
import org.locationtech.jts.geom.Coordinate

object TargetObsQueries {

  case class TargetWithObs(target: SiderealTarget, obs: List[ExploreObservation])

  @Lenses
  case class TargetsWithObs(targets: List[SiderealTarget], obs: List[ExploreObservation])

  implicit val targetWithObsDecoder = new Decoder[TargetWithObs] {
    final def apply(c: HCursor): Decoder.Result[TargetWithObs] =
      for {
        target  <- c.as[SiderealTarget]
        obsJson <- c.downField("observations").as[List[Json]].map(_.map(_.hcursor))
        obsList <- obsJson.traverse(obsDecoder(target).apply)
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

  object AddTarget extends GraphQLQuery {
    val document = """
      mutation($target: targets_insert_input!) {
        insert_targets_one(object: $target) {
          id
        }
      }
    """

    case class Variables(target: SiderealTarget)
    object Variables { implicit val jsonEncoder: Encoder[Variables] = deriveEncoder[Variables] }

    case class Result(id: SiderealTarget.Id)
    object Result { implicit val jsonDecoder: Decoder[Result] = deriveDecoder[Result] }

    case class Data(insert_targets_one: Result)
    object Data { implicit val jsonDecoder: Decoder[Data] = deriveDecoder[Data] }

    implicit val varEncoder: Encoder[Variables] = Variables.jsonEncoder
    implicit val dataDecoder: Decoder[Data]     = Data.jsonDecoder
  }

  object RemoveTarget extends GraphQLQuery {
    val document = """
      mutation ($id: uuid!) {
        delete_targets_by_pk(id: $id) {
          id
        }
      }
    """

    case class Variables(id: SiderealTarget.Id)
    object Variables { implicit val jsonEncoder: Encoder[Variables] = deriveEncoder[Variables] }

    case class Result(id: SiderealTarget.Id)
    object Result { implicit val jsonDecoder: Decoder[Result] = deriveDecoder[Result] }

    case class Data(delete_targets_by_pk: Result)
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

  def insertTarget(target: SiderealTarget): IO[Unit] =
    AppCtx.flatMap(
      _.clients.programs
        .query(AddTarget)(AddTarget.Variables(target).some)
        .void
    )

  def removeTarget(id: SiderealTarget.Id): IO[Unit] =
    AppCtx.flatMap(
      _.clients.programs
        .query(RemoveTarget)(RemoveTarget.Variables(id).some)
        .void
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
