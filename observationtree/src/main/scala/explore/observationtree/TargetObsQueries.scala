// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all._
import clue.GraphQLOperation
import clue.macros.GraphQL
import explore.AppCtx
import explore.GraphQLSchemas.ExploreDB.Types._
import explore.GraphQLSchemas._
import explore.components.graphql.SubscriptionRenderMod
import explore.data.KeyedIndexedList
import explore.implicits._
import explore.model.ExploreObservation
import explore.model.ObsSummary
import explore.model.SiderealTarget
import explore.model.decoders._
import explore.model.reusability._
import io.circe.Decoder
import io.circe.HCursor
import io.circe.Json
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses

object TargetObsQueries {

  case class TargetWithObs(target: SiderealTarget, obs: List[ObsSummary])

  type TargetList = KeyedIndexedList[SiderealTarget.Id, SiderealTarget]
  type ObsList    = KeyedIndexedList[ExploreObservation.Id, ObsSummary]

  @Lenses
  case class TargetsWithObs(targets: TargetList, obs: ObsList)

  implicit val targetWithObsDecoder = new Decoder[TargetWithObs] {
    final def apply(c: HCursor): Decoder.Result[TargetWithObs] =
      for {
        target  <- c.as[SiderealTarget]
        obsJson <- c.downField("observations").as[List[Json]].map(_.map(_.hcursor))
        obsList <- obsJson.traverse(ObsSummary.decoderForTarget(target).apply)
      } yield TargetWithObs(target, obsList)
  }

  implicit val targetsWithObsDecoder = new Decoder[TargetsWithObs] {
    final def apply(c: HCursor): Decoder.Result[TargetsWithObs] =
      c.as[List[TargetWithObs]].map { targetsWithObs =>
        TargetsWithObs(
          KeyedIndexedList.fromList(targetsWithObs.map(_.target), SiderealTarget.id.get),
          KeyedIndexedList.fromList(targetsWithObs.flatMap(_.obs), ObsSummary.id.get)
        )
      }
  }

  @GraphQL
  object Subscription extends GraphQLOperation[ExploreDB] {
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
              id
              name
            }
            duration_seconds
          }
        }
      }
    """

    @Lenses
    case class Data(targets: TargetsWithObs)
  }

  @GraphQL
  object ObsMutation extends GraphQLOperation[ExploreDB] {
    val document = """
      mutation ($id: uuid!, $fields: observations_set_input!){
        update_observations(_set: $fields, where: {id: {_eq: $id}}) {
          affected_rows
        }
      }
    """
  }

  @GraphQL(debug = false)
  object AddTarget extends GraphQLOperation[ExploreDB] {
    val document = """
      mutation($target: targets_insert_input!) {
        insert_targets_one(object: $target) {
          id
        }
      }
    """
  }

  @GraphQL
  object RemoveTarget extends GraphQLOperation[ExploreDB] {
    val document = """
      mutation ($id: uuid!) {
        delete_targets_by_pk(id: $id) {
          id
        }
      }
    """
  }

  def mutateObs(obsId: ExploreObservation.Id, fields: ObservationsSetInput): IO[Unit] =
    AppCtx.flatMap(implicit ctx => ObsMutation.execute(obsId, fields).void)

  def insertTarget(target: SiderealTarget): IO[Unit] =
    AppCtx.flatMap(implicit ctx => AddTarget.execute(target).void)

  def removeTarget(id: SiderealTarget.Id): IO[Unit] =
    AppCtx.flatMap(implicit ctx => RemoveTarget.execute(id).void)

  implicit val targetsWithObsReusability: Reusability[TargetsWithObs] = Reusability.derive

  type SubscriptionRenderer =
    (View[TargetsWithObs] => VdomNode) => SubscriptionRenderMod[Subscription.Data, TargetsWithObs]

  val TargetObsSubscription: SubscriptionRenderer =
    render =>
      AppCtx.withCtx { implicit appCtx =>
        SubscriptionRenderMod[Subscription.Data, TargetsWithObs](
          Subscription.subscribe(),
          _.map(
            Subscription.Data.targets.get
          )
        )(render)
      }
}
