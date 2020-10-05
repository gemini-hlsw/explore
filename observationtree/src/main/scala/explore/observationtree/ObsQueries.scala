// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import clue.GraphQLOperation
import explore.AppCtx
import explore.components.graphql.SubscriptionRenderMod
import explore.implicits._
import explore.model.ObsSummary
import explore.model.reusability._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.macros.Lenses
import clue.macros.GraphQL
import explore.GraphQLSchemas._

object ObsQueries {

  // We will eventually need a structure to store the whole Observation info but only summaries of target/constraints/configuration.

  @GraphQL
  object Subscription extends GraphQLOperation[ObservationDB] {
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

    @Lenses
    case class Data(observations: List[ObsSummary])
  }

  type SubscriptionRenderer =
    (
      View[List[ObsSummary]] => VdomNode
    ) => SubscriptionRenderMod[Subscription.Data, List[ObsSummary]]

  val ObsSubscription: SubscriptionRenderer =
    render =>
      AppCtx.withCtx { implicit appCtx =>
        SubscriptionRenderMod[Subscription.Data, List[ObsSummary]](
          Subscription.subscribe(),
          _.map(
            Subscription.Data.observations.get
          )
        )(render)
      }
}
