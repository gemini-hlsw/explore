// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB

// gql: import io.circe.refined.*

object ProposalQueriesGQL:
  @GraphQL
  trait CreateProposalMutation extends GraphQLOperation[ObservationDB]:
    val document: String = s"""
      mutation($$input: CreateProposalInput!) {
        createProposal(input: $$input) {
          proposal {
            title
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
            title
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
  trait UnlinkUser extends GraphQLOperation[ObservationDB]:
    val document: String = s"""
      mutation ($$input: UnlinkUserInput!) {
        unlinkUser(input: $$input) {
          result
        }
      }
    """
