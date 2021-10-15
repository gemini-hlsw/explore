// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
// gql: import io.circe.refined._
// gql: import lucuma.ui.reusability._

object TargetEnvQueriesGQL {

  @GraphQL
  trait TargetEnvQuery extends GraphQLOperation[ObservationDB] {
    val document = """
      query($id: TargetEnvironmentId!) {
        targetEnvironment(targetEnvironmentId: $id) {
          scienceTargets {
            id
            name
            magnitudes {
              band
            }
          }
        }
      }
      """
  }

  @GraphQL
  trait TargetEnvEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription($id: TargetEnvironmentId!) {
        targetEnvironmentEdit(targetEnvironmentId: $id) {
          id
        }
      }
      """
  }

  @GraphQL
  trait ProgramTargetEnvEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription {
        targetEnvironmentEdit(programId: "p-2") {
          id
        }
      }
      """
  }
}
