// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.core.model
import lucuma.schemas.ObservationDB

// gql: import explore.model.reusability._
// gql: import io.circe.refined._
// gql: import lucuma.schemas.decoders._
// gql: import lucuma.ui.reusability._

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
          type Execution = model.NonNegDuration
        }
      }
    }
  }

  @GraphQL
  trait ProgramEditSubscription extends GraphQLOperation[ObservationDB] {
    val document: String = """
      subscription($programId: ProgramId) {
        programEdit(programId: $programId) {
          id
        }
      }
    """
  }
}
