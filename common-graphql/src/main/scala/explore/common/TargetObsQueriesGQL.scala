// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model.ConstraintsSummary
import explore.schemas.ObservationDB
// gql: import io.circe.refined._
// gql: import lucuma.ui.reusability._

object TargetObsQueriesGQL {

  @GraphQL
  trait TargetsObsQuery extends GraphQLOperation[ObservationDB] {
    val document = """
      query($first: Int = 2147483647) {
        targets(programId: "p-2", first: $first) {
          nodes {
            id
            name
          }
        }

        asterisms(programId: "p-2", first: $first) {
          nodes {
            id
            name
            targets(first: 2147483647) {
              nodes {
                id
                name
              }
            }
          }
        }

        observations(programId: "p-2", first: $first) {
          nodes {
            id
            pointing: observationTarget {
              type: __typename
              ... on Target {
                targetId: id
              }
              ... on Asterism {
                asterismId: id
              }
            }
            constraintSet {
              id
              name
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
  trait TargetEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription {
        targetEdit(programId: "p-2") {
          id
        }
      }
    """
  }

  @GraphQL
  trait AsterismEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription {
        asterismEdit(programId: "p-2") {
          id
        }
      }
    """
  }

  @GraphQL
  trait UpdateObservationMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: EditObservationInput!) {
        updateObservation(input: $input) {
          id
        }
      }
    """
  }

  @GraphQL
  trait AddTarget extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($targetId: TargetId!, $name: NonEmptyString!) {
        createSiderealTarget(input:{
          targetId: $targetId,
          name: $name,
          programIds: ["p-2"],
          ra: {microarcseconds: 0},
          dec: {microarcseconds: 0}
        }) {
          id
        }
      }
    """
  }

  @GraphQL
  trait RemoveTarget extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($targetId: TargetId!) {
        deleteTarget(targetId: $targetId) {
          id
        }
      }
    """
  }

  @GraphQL
  trait UndeleteTarget extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($targetId: TargetId!) {
        undeleteTarget(targetId: $targetId) {
          id
        }
      }
    """
  }

  @GraphQL
  trait AddAsterism extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($asterismId: AsterismId!, $name: NonEmptyString) {
        createAsterism(input:{
          asterismId: $asterismId,
          name: $name,
          programIds: ["p-2"]
        }) {
          id
        }
      }
    """
  }

  @GraphQL
  trait RemoveAsterism extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($asterismId: AsterismId!) {
        deleteAsterism(asterismId: $asterismId) {
          id
        }
      }
    """
  }

  @GraphQL
  trait UndeleteAsterism extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($asterismId: AsterismId!) {
        undeleteAsterism(asterismId: $asterismId) {
          id
        }
      }
    """
  }

  @GraphQL
  trait AssignTargetToObs extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($targetId: TargetId!, $obsId: ObservationId!) {
        updatePointing(
          input: { targetId: $targetId, observationIds: [$obsId] }
        ) {
          id
        }
      }
    """
  }

  @GraphQL
  trait AssignAsterismToObs extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($asterismId: AsterismId!, $obsId: ObservationId!) {
        updatePointing(
          input: { asterismId: $asterismId, observationIds: [$obsId] }
        ) {
          id
        }
      }
    """
  }

  @GraphQL
  trait UnassignObs extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($obsId: ObservationId!) {
        updateObservation(
          input: { observationId: $obsId, asterismId: null, targetId: null }
        ) {
          id
        }
      }
    """
  }

  @GraphQL
  trait ShareTargetWithAsterisms extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($targetId: TargetId!, $asterismId: AsterismId!) {
        shareTargetWithAsterisms(input: { targetId: $targetId, asterismIds: [$asterismId] }) {
          id
        }
      }
    """
  }

  @GraphQL
  trait UnshareTargetWithAsterisms extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($targetId: TargetId!, $asterismId: AsterismId!) {
        unshareTargetWithAsterisms(input: { targetId: $targetId, asterismIds: [$asterismId] }) {
          id
        }
      }
    """
  }
}
