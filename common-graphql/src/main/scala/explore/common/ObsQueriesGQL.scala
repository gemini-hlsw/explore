// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model
import explore.model.ConstraintsSummary
import explore.schemas.ObservationDB
// gql: import explore.model.reusability._
// gql: import io.circe.refined._
// gql: import lucuma.ui.reusability._

object ObsQueriesGQL {

  @GraphQL
  trait ProgramObservationsQuery extends GraphQLOperation[ObservationDB] {
    val document = """
      query {
        observations(programId: "p-2") {
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
              imageQuality
              cloudExtinction
              skyBackground
              waterVapor
            }
          }
        }
      }
    """

    object Data {
      object Observations {
        object Nodes {
          trait ConstraintSet extends ConstraintsSummary
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

  @GraphQL
  trait ObsEditQuery extends GraphQLOperation[ObservationDB] {
    val document = """
      query($obsId: ObservationId!) {
        observation(observationId: $obsId) {
          id
          constraintSet {
            name
            cloudExtinction
            imageQuality
            skyBackground
            waterVapor
            elevationRange {
              type: __typename
              ... on AirMassRange {
                min
                max
              }
              ... on HourAngleRange {
                minHours
                maxHours
              }
            }
          }
        }
      }
    """

    object Data {
      object Observation {
        object ConstraintSet {
          type ElevationRange = model.ElevationRange
        }
      }
    }
  }

  @GraphQL
  trait ObservationEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription($obsId: ObservationId!) {
        observationEdit(observationId: $obsId) {
          id
        }
      }
    """
  }

  @GraphQL
  trait UpdateConstraintSetMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation ($obsId: ObservationId!, $input: EditConstraintSetInput!){
        updateConstraintSet(input: {observationIds: [$obsId], constraintSet: $input}) {
          id
        }
      }
    """
  }

}
