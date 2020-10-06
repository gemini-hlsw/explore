// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.semanticui.sizes.Huge
import clue.GraphQLOperation
import explore.GraphQLSchemas.LucumaODB
import clue.macros.GraphQL
import explore.components.graphql.SubscriptionRender
import explore.implicits._

object UnderConstruction {

  @GraphQL(debug = false)
  object LucumaTestSubscription extends GraphQLOperation[LucumaODB] {
    val document =
      """subscription {
        targetEdited {
          id
          oldValue {
            name
          }
          newValue {
            name
          }
        }
      }"""
  }

  protected val component =
    ScalaComponent
      .builder[Unit]
      .stateless
      .render { _ =>
        <.div(
          ExploreStyles.HVCenter,
          <.div(
            <.div("Under Construction"),
            <.div(ExploreStyles.HVCenter, Icons.Cogs.copy(size = Huge)),
            AppCtx.withCtx(implicit ctx =>
              SubscriptionRender[LucumaTestSubscription.Data,
                                 LucumaTestSubscription.Data.TargetEdited
              ](
                LucumaTestSubscription.subscribe(),
                _.map(
                  LucumaTestSubscription.Data.targetEdited.get _
                )
              )(data => <.div(data.toString))
            )
          )
        )
      }
      .build

  def apply() = component()

}
