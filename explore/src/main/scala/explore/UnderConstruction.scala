// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.data.NonEmptyList
import clue.GraphQLOperation
import clue.macros.GraphQL
import explore.GraphQLSchemas.ObservationDB
import explore.Icons
import explore.components.graphql.LiveQueryRender
import explore.components.ui.ExploreStyles
import explore.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.semanticui.sizes.Huge

object UnderConstruction {

  @GraphQL(debug = false)
  object LucumaTestQuery extends GraphQLOperation[ObservationDB] {
    val document =
      """query {
          target(id: "t-2") {
            id
            name
          }
        }"""
  }

  @GraphQL(debug = false)
  object LucumaTestSubscription extends GraphQLOperation[ObservationDB] {
    val document =
      """subscription {
        targetEdited(id: "p-2") {
          id
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
            AppCtx.withCtx { implicit ctx =>
              LucumaTestSubscription.subscribe[cats.effect.IO]().unsafeRunAsync(println)

              LiveQueryRender[LucumaTestQuery.Data, LucumaTestQuery.Data.Target](
                LucumaTestQuery.query(),
                _.target.get,
                NonEmptyList.of(
                  LucumaTestSubscription.subscribe()
                )
              )(data => <.div(data.toString))
            }
          )
        )
      }
      .build

  def apply() = component()

}
