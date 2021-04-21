// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model.ConstraintsSummary
import explore.schemas.ObservationDB
// gql: import explore.model.reusability._
// gql: import io.circe.refined._
// gql: import lucuma.ui.reusability._

object ObsQueriesGQL {

  @GraphQL
  trait ProgramObservationsQuery extends GraphQLOperation[ObservationDB] {
    val document = """
      query($first: Int = 2147483647) {
        observations(programId: "p-2", first: $first) {
          nodes {
            id
            observationTarget {
              type: __typename
              ... on Target {
                targetId: id
                targetName: name
              }
              ... on Asterism {
                asterismId: id
                asterismName: name
              }
            }
            constraintSet {
              id
              name
              imageQuality
              cloudExtinction
              skyBackground
              waterVapor
              obsCount: observations(first: 0) { 
                totalCount
              }
            }
          }
        }
      }
    """

    object Data {
      object Observations {
        object Nodes {
          type ConstraintSet = ConstraintsSummary
        }
      }
    }

  }

  @GraphQL
  trait ProgramObservationsEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription {
        observationEdit(programId:"p-2") {
          id
        }
      }
    """
  }

  @GraphQL
  trait ProgramCreateObservation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($createObservation: CreateObservationInput!) {
        createObservation(input: $createObservation) {
          id
        }
      }
    """
  }

  @GraphQL
  trait ProgramDeleteObservation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($oid: ObservationId!) {
        deleteObservation(observationId: $oid) {
          id
        }
      }
    """
  }

  @GraphQL
  trait ProgramUndeleteObservation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($oid: ObservationId!) {
        undeleteObservation(observationId: $oid) {
          id
        }
      }
    """
  }

}
