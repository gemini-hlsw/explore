// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB

// gql: import explore.model.reusability._
// gql: import lucuma.ui.reusability._
// gql: import io.circe.refined._

object ProgramQueriesGQL {
  @GraphQL
  trait ProgramsQuery extends GraphQLOperation[ObservationDB] {
    val document: String = """
      query($includeDeleted: Boolean!) {
        programs(includeDeleted: $includeDeleted) {
          nodes {
            id
            name
            existence
          }
        }
      }
    """
  }

  @GraphQL
  trait CreateProgramMutation extends GraphQLOperation[ObservationDB] {
    val document: String = """
      mutation($input: CreateProgramInput!) {
        createProgram(input: $input) {
          id
          name
        }
      }
    """
  }

  @GraphQL
  trait EditProgramMutation extends GraphQLOperation[ObservationDB] {
    val document: String = """
      mutation($input: EditProgramInput!) {
        editProgram(input: $input) {
          id
        }
      }
    """
  }

  @GraphQL
  trait ProgramEditSubscription extends GraphQLOperation[ObservationDB] {
    val document: String = """
      subscription {
        programEdit {
          id
        }
      }
    """
  }
}
