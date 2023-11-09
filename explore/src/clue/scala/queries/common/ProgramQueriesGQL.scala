// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.core.model
import lucuma.core.util.TimeSpan
import lucuma.schemas.ObservationDB
// gql: import lucuma.odb.json.time.decoder.given
// gql: import lucuma.schemas.decoders.given
// gql: import explore.DynamicEnums.given

object ProgramQueriesGQL {
  @GraphQL
  trait CreateProgramMutation extends GraphQLOperation[ObservationDB] {
    val document: String = s"""
      mutation($$input: CreateProgramInput!) {
        createProgram(input: $$input) {
          program $ProgramInfoSubquery
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
  trait UpdateObsAttachmentMutation extends GraphQLOperation[ObservationDB] {
    val document: String = """
    mutation($input: UpdateObsAttachmentsInput!) {
      updateObsAttachments(input: $input) {
        obsAttachments {
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
          plannedTimeRange {
            maximum {
              total {
                microseconds
              }
            }
          }
        }
      }
    """

    object Data:
      object Program:
        type Proposal = model.Proposal
        object PlannedTimeRange:
          object Maximum:
            type Total = TimeSpan
  }

  @GraphQL
  trait ProgramGroupsQuery extends GraphQLOperation[ObservationDB] {
    val document: String = s"""#graphql
      query ($$programId: ProgramId!) {
        program(programId: $$programId) {
          allGroupElements ${GroupQueriesGQL.GroupElementsSubQuery}
        }
      }
    """
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

  @GraphQL
  trait GroupEditSubscription extends GraphQLOperation[ObservationDB] {
    val document: String = s"""
      subscription($$programId: ProgramId!) {
        groupEdit(input: { programId: $$programId }) {
          value ${GroupQueriesGQL.OptionalGroupSubQuery}
          editType
        }
      }
    """
  }

  @GraphQL
  trait ProgramEditAttachmentSubscription extends GraphQLOperation[ObservationDB] {
    val document: String = s"""
      subscription($$programId: ProgramId!) {
        programEdit(input: {programId: $$programId}) {
          value {
            obsAttachments $ObsAttachmentSubquery
            proposalAttachments $ProposalAttachmentSubquery
          }
        }
      }
    """
  }

  @GraphQL
  trait ProgramInfoDelta extends GraphQLOperation[ObservationDB] {
    val document = s"""
      subscription {
        programEdit {
          value $ProgramInfoSubquery
        }
      }
    """
  }
}
