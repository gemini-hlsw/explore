// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.core.model
import lucuma.core.util.TimeSpan
import lucuma.schemas.ObservationDB

// gql: import io.circe.refined.*
// gql: import lucuma.schemas.decoders.*

object ProgramQueriesGQL {
  @GraphQL
  trait ProgramsQuery extends GraphQLOperation[ObservationDB] {
    val document: String = """
      query($includeDeleted: Boolean!) {
        programs(includeDeleted: $includeDeleted) {
          matches {
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
          program {
            id
            name
          }
        }
      }
    """
  }

  @GraphQL
  trait UpdateProgramsMutation extends GraphQLOperation[ObservationDB] {
    val document: String = """
      mutation($input: UpdateProgramsInput!) {
        updatePrograms(input: $input) {
          programs {
            id
          }
        }
      }
    """
  }

  @GraphQL
  trait ProgramProposalQuery extends GraphQLOperation[ObservationDB] {
    val document: String = """
      query($programId: ProgramId!) {
        program(programId: $programId) {
          proposal {
            title
            proposalClass {
              __typename
              minPercentTime
              ... on LargeProgram {
                minPercentTotalTime
                totalTime {
                  microseconds
                }
              } 
              ... on Intensive {
                minPercentTotalTime
                totalTime {
                  microseconds
                }
              }
            }
            category
            toOActivation
            abstract
            partnerSplits {
              partner
              percent
            }
          }
          plannedTime {
            execution {
              microseconds
            }
          }
        }
      }
    """

    object Data {
      object Program {
        type Proposal = model.Proposal
        object PlannedTime {
          type Execution = TimeSpan
        }
      }
    }
  }

  @GraphQL
  trait ProgramEditSubscription extends GraphQLOperation[ObservationDB] {
    val document: String = """
      subscription($programId: ProgramId) {
        programEdit(input: {programId: $programId}) {
          value {
            id
          }
        }
      }
    """
  }
}
