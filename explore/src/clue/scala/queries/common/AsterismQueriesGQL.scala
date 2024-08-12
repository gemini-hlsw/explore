// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB

object AsterismQueriesGQL {
  @GraphQL
  trait UpdateAsterismsMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: UpdateAsterismsInput!) {
        updateAsterisms(input: $input) {
          observations {
            id
          }
        }
      }
    """
  }

  // Group these 2 mutations together so that the targetEdit and observationEdit subscriptions come
  // close together and get grouped by the programCache so we don't have an invalid state for the UI.
  @GraphQL
  trait UpdateTargetsAndAsterismsMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($targetInput: UpdateTargetsInput!,$asterismInput: UpdateAsterismsInput!) {
        updateTargets(input: $targetInput) {
          targets {
            id
          }
        }

        updateAsterisms(input: $asterismInput) {
          observations {
            id
          }
        }
      }
    """
  }
}
