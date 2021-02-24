// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.data.NonEmptyList
import cats.effect.IO
import clue.GraphQLOperation
import clue.macros.GraphQL
import explore.AppCtx
import explore.GraphQLSchemas._
import explore.components.graphql.LiveQueryRenderMod
import explore.implicits._
import explore.model.ObsSummary
import explore.model.reusability._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.reusability._

object ObsQueries {

  @GraphQL
  object ProgramObservationsQuery extends GraphQLOperation[ObservationDB] {
    val document = """
      query {
        observations(programId: "p-2") {
          id
          name
          observationTarget {
            ... on Asterism {
              asterism_id: id
            }
            ... on Target {
              target_id: id
            }
          }
        }
      }
    """

    object Data {
      type Observations = ObsSummary
    }
  }

  @GraphQL
  object ProgramObservationsEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription {
        observationEdit(programId:"p-2") {
          id
        }
      }
    """
  }

  type LiveQueryRenderer =
    (
      View[List[ObsSummary]] => VdomNode
    ) => LiveQueryRenderMod[ObservationDB, ProgramObservationsQuery.Data, List[ObsSummary]]

  val ObsLiveQuery: LiveQueryRenderer =
    render =>
      AppCtx.withCtx { implicit appCtx =>
        LiveQueryRenderMod[ObservationDB, ProgramObservationsQuery.Data, List[ObsSummary]](
          ProgramObservationsQuery.query(),
          _.observations,
          NonEmptyList.of(ProgramObservationsEditSubscription.subscribe[IO]())
        )(render)
      }

  @GraphQL
  object ProgramCreateObservations extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation CreateObservation($createObservation: CreateObservationInput!) {
        createObservation(input: $createObservation) {
          id
        }
      }
    """
  }
}
