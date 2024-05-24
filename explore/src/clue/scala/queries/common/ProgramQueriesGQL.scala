// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
import explore.model.Proposal

import GroupQueriesGQL.*

object ProgramQueriesGQL:
  @GraphQL
  trait CreateProgramMutation extends GraphQLOperation[ObservationDB]:
    val document: String = s"""
      mutation($$input: CreateProgramInput!) {
        createProgram(input: $$input) {
          program $ProgramInfoSubquery
        }
      }
    """

  @GraphQL
  trait UpdateProgramsMutation extends GraphQLOperation[ObservationDB]:
    val document: String = """
      mutation($input: UpdateProgramsInput!) {
        updatePrograms(input: $input) {
          programs {
            id
          }
        }
      }
    """

  @GraphQL
  trait UpdateObsAttachmentMutation extends GraphQLOperation[ObservationDB]:
    val document: String = """
    mutation($input: UpdateObsAttachmentsInput!) {
      updateObsAttachments(input: $input) {
        obsAttachments {
          id
        }
      }
    }
   """

  @GraphQL
  trait ProgramGroupsQuery extends GraphQLOperation[ObservationDB]:
    val document: String = s"""#graphql
      query ($$programId: ProgramId!) {
        program(programId: $$programId) {
          allGroupElements $GroupElementsSubQuery
        }
      }
    """

  @GraphQL
  trait GroupEditSubscription extends GraphQLOperation[ObservationDB]:
    val document: String = s"""
      subscription($$input: ProgramEditInput!) {
        groupEdit(input: $$input) {
          value $GroupSubQuery
          meta:value {
            existence
          }
          editType
        }
      }
    """

  @GraphQL
  trait ProgramEditAttachmentSubscription extends GraphQLOperation[ObservationDB]:
    val document: String = s"""
      subscription($$input: ProgramEditInput!) {
        programEdit(input: $$input) {
          value {
            obsAttachments $ObsAttachmentSubquery
            proposalAttachments $ProposalAttachmentSubquery
          }
        }
      }
    """

  // TODO: We now have 2 bits of ProgramSummaries that start a `programEdit` subscription
  // for the current program. The program attachments could be moved into the program details
  // to avoid this. But, we also need to change the ODB so that editing a proposal triggers a
  // subscription update.
  @GraphQL
  trait ProgramEditDetailsSubscription extends GraphQLOperation[ObservationDB]:
    val document: String = s"""
      subscription($$input: ProgramEditInput!) {
        programEdit(input: $$input) {
          value $ProgramDetailsSubquery
        }
      }
    """

  @GraphQL
  trait ProgramInfoDelta extends GraphQLOperation[ObservationDB]:
    val document = s"""
      subscription {
        programEdit {
          value $ProgramInfoSubquery
        }
      }
    """

