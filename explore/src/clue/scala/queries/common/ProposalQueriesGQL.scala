// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB

object ProposalQueriesGQL:
  @GraphQL
  trait CreateProposalMutation extends GraphQLOperation[ObservationDB]:
    val document: String = s"""
      mutation($$input: CreateProposalInput!) {
        createProposal(input: $$input) {
          proposal {
            category
          }
        }
      }
    """

  @GraphQL
  trait UpdateProposalMutation extends GraphQLOperation[ObservationDB]:
    val document: String = s"""
      mutation($$input: UpdateProposalInput!) {
        updateProposal(input: $$input) {
          proposal {
            call {
              id
            }
          }
        }
      }
    """

  @GraphQL
  trait SetProposalStatus extends GraphQLOperation[ObservationDB]:
    val document: String = s"""
      mutation($$input: SetProposalStatusInput!) {
        setProposalStatus(input: $$input) {
          program {
            id
          }
        }
      }
    """

  @GraphQL
  trait AddProgramUser extends GraphQLOperation[ObservationDB]:
    val document: String = s"""
      mutation($$input: AddProgramUserInput!) {
        addProgramUser(input: $$input) {
          programUser $ProgramUserSubquery
        }
      }
    """

  @GraphQL
  trait DeleteProgramUser extends GraphQLOperation[ObservationDB]:
    val document: String = s"""
      mutation($$input: DeleteProgramUserInput!) {
        deleteProgramUser(input: $$input) {
          result
        }
      }
    """

  // TODO: Do we need this?
  @GraphQL
  trait UnlinkUser extends GraphQLOperation[ObservationDB]:
    val document: String = s"""
      mutation ($$input: UnlinkUserInput!) {
        unlinkUser(input: $$input) {
          result
        }
      }
    """
