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

object ConstraintSetObsQueriesGQL {
  @GraphQL
  trait ConstraintSetsObsQuery extends GraphQLOperation[ObservationDB] {
    val document = """
      query($first: Int = 2147483647) {
        constraintSets(programId: "p-2", first: $first) {
          nodes {
            id
            name
            imageQuality
            cloudExtinction
            skyBackground
            waterVapor
          }
        }

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
            }
          }
        }
      }
    """

    object Data {
      object ConstraintSets {
        type Nodes = ConstraintsSummary
      }

      object Observations {
        object Nodes {
          type ConstraintSet = ConstraintsSummary
        }
      }
    }
  }

  @GraphQL
  trait ConstraintSetEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription {
        constraintSetEdit(programId: "p-2") {
          id
        }
      }
    """
  }

  @GraphQL
  trait AddConstraintSet extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: CreateConstraintSetInput!) {
        createConstraintSet(input: $input) {
          id
        }
      }
    """
  }

  @GraphQL
  trait DeleteConstraintSet extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($constraintSetId: ConstraintSetId!) {
        deleteConstraintSet(constraintSetId: $constraintSetId) {
          id
        }
      }
    """
  }

  @GraphQL
  trait UndeleteConstraintSet extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($constraintSetId: ConstraintSetId!) {
        undeleteContraintSet(constraintSetId: $constraintSetId) {
          id
        }
      }
    """
  }

  @GraphQL
  trait AssignConstraintSetToObs extends GraphQLOperation[ObservationDB] {
    val document: String = """
      mutation($constraintSetId: ConstraintSetId!, $obsId: ObservationId!) {
        updateObservationConstraintSet(
          input: { constraintSetId: $constraintSetId, observationIds: [$obsId] }
        ) {
          id
        }
      }
    """
  }

  @GraphQL
  trait UnassignConstraintSetFromObs extends GraphQLOperation[ObservationDB] {
    val document: String = """
      mutation($obsId: ObservationId!) {
        updateObservationConstraintSet(
          input: { observationIds: [$obsId], constraintSetId: null }
         ) {
          id
        }
      }
    """
  }
}
