// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import clue.GraphQLQuery
import explore.AppCtx
import explore.components.graphql.SubscriptionRenderMod
import explore.implicits._
import explore.model.ObsSummary
import explore.model.reusability._
import io.circe.Decoder
import io.circe.Encoder
import io.circe.generic.semiauto.deriveDecoder
import io.circe.generic.semiauto.deriveEncoder
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses

object ObsQueries {

  // We will eventually need a structure to store the whole Observation info but only summaries of target/constraints/configuration.

  object Subscription extends GraphQLQuery {
    val document = """
      subscription {
        observations {
          id
          status
          target {
            id
            name
          }
          configuration
          constraint {
            id
            name
          }
          duration_seconds
        }
      }
    """

    case class Variables()
    object Variables { implicit val jsonEncoder: Encoder[Variables] = deriveEncoder[Variables] }

    @Lenses
    case class Data(observations: List[ObsSummary])
    object Data { implicit val jsonDecoder: Decoder[Data] = deriveDecoder[Data] }

    implicit val varEncoder: Encoder[Variables] = Variables.jsonEncoder
    implicit val dataDecoder: Decoder[Data]     = Data.jsonDecoder
  }

  type SubscriptionRenderer =
    (
      View[List[ObsSummary]] => VdomNode
    ) => SubscriptionRenderMod[Subscription.Data, List[ObsSummary]]

  val ObsSubscription: SubscriptionRenderer =
    render =>
      AppCtx.withCtx { implicit appCtx =>
        SubscriptionRenderMod[Subscription.Data, List[ObsSummary]](
          appCtx.clients.odb
            .subscribe(Subscription)(),
          _.map(
            Subscription.Data.observations.get
          )
        )(render)
      }
}
