// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
import lucuma.schemas.model as schemasModel
import lucuma.schemas.odb.*
// gql: import lucuma.schemas.decoders.given

object TargetQueriesGQL {

  @GraphQL
  trait TargetNameQuery extends GraphQLOperation[ObservationDB] {
    // FIXME Change this to an actual name pattern query when it's available in the API
    val document = s"""
      query($$programId: ProgramId!) {
        targetGroup(programId: $$programId) {
          matches {
            target $TargetWithIdSubquery
          }
        }
      }
    """
  }

  @GraphQL
  trait CreateTargetMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: CreateTargetInput!) {
        createTarget(input: $input) {
          target {
            id
          }
        }
      }
    """
  }

  @GraphQL
  trait UpdateTargetsMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: UpdateTargetsInput!) {
        updateTargets(input: $input) {
          targets {
            id
          }
        }
      }
    """
  }

  @GraphQL
  trait UpdateTargetsMutationWithResult extends GraphQLOperation[ObservationDB] {
    val document = s"""
      mutation($$input: UpdateTargetsInput!) {
        updateTargets(input: $$input) {
          targets $TargetWithIdSubquery
        }
      }
    """

    object Data {
      object UpdateTargets {
        type Targets = schemasModel.TargetWithId
      }
    }
  }

  @GraphQL
  trait CloneTargetMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: CloneTargetInput!) {
        cloneTarget(input: $input) {
          newTarget {
            id
          }
        }
      }
    """
  }

  @GraphQL
  trait TargetEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription($targetId: TargetId!) {
        targetEdit(input: {targetId: $targetId}) {
          id
        }
      }
    """
  }

  @GraphQL
  trait ProgramTargetsDelta extends GraphQLOperation[ObservationDB] {
    val document = s"""
      subscription($$input: TargetEditInput!) {
        targetEdit(input: $$input) {
          value $TargetWithIdSubquery
          meta:value {
            existence
          }
        }
      }
    """
  }
}
