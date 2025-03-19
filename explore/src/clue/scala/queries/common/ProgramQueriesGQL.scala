// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB

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
  trait UpdateAttachmentMutation extends GraphQLOperation[ObservationDB]:
    val document: String = """
    mutation($input: UpdateAttachmentsInput!) {
      updateAttachments(input: $input) {
        attachments {
          id
        }
      }
    }
   """

  @GraphQL
  trait ProgramGroupsQuery extends GraphQLOperation[ObservationDB]:
    val document: String = s"""
      query ($$programId: ProgramId!) {
        program(programId: $$programId) {
          allGroupElements {
            group $GroupSubQuery
          }
        }
      }
    """

  @GraphQL
  trait GroupEditSubscription extends GraphQLOperation[ObservationDB]:
    val document: String = s"""
      subscription($$input: ProgramEditInput!) {
        groupEdit(input: $$input) {
          value $GroupSubQuery
          meta:value { existence }
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
            attachments $AttachmentSubquery
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

  @GraphQL
  trait ConfigurationRequestSubscription extends GraphQLOperation[ObservationDB]:
    val document = s"""
      subscription($$input: ConfigurationRequestEditInput!) {
        configurationRequestEdit(input: $$input) {
          configurationRequest $ConfigurationRequestSubquery
        }
      }
    """

  @GraphQL
  trait ProgramUsersMutation extends GraphQLOperation[ObservationDB]:
    val document = s"""
      mutation($$input: UpdateProgramUsersInput!) {
        updateProgramUsers(input: $$input) {
          programUsers {
            user {
              id
            }
          }
        }
      }
      """

  @GraphQL
  trait ChangeProgramUserRoleMutation extends GraphQLOperation[ObservationDB]:
    val document = s"""
      mutation($$input: ChangeProgramUserRoleInput!) {
        changeProgramUserRole(input: $$input) {
          programUser {
            role
          }
        }
      }
    """

  @GraphQL
  trait UpdateConfigurationRequestsMutation extends GraphQLOperation[ObservationDB]:
    val document = s"""
      mutation($$input: UpdateConfigurationRequestsInput!) {
        updateConfigurationRequests(input: $$input) {
          requests {
            id
          }
        }
      }
      """

  @GraphQL
  trait ResolveProgramReference extends GraphQLOperation[ObservationDB]:
    val document = """
      query($input: ProgramReferenceLabel) {
        program(programReference: $input) {
          id
        }
      }
    """

  @GraphQL
  trait ResolveProposalReference extends GraphQLOperation[ObservationDB]:
    val document = """
      query($input: ProposalReferenceLabel) {
        program(proposalReference: $input) {
          id
        }
      }
    """

  @GraphQL
  trait CreateProgramNoteMutation extends GraphQLOperation[ObservationDB]:
    val document = s"""
      mutation($$input: CreateProgramNoteInput!) {
        createProgramNote(input: $$input) {
          programNote {
            id
          }
        }
      }
    """

  @GraphQL
  trait UpdateProgramNotesMutation extends GraphQLOperation[ObservationDB]:
    val document = s"""
      mutation($$input: UpdateProgramNotesInput!) {
        updateProgramNotes(input: $$input) {
          programNotes {
            id
          }
        }
      }
    """
