// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
import explore.model.CoIInvitation

object InvitationQueriesGQL:
  @GraphQL
  trait CreateInviteMutation extends GraphQLOperation[ObservationDB]:
    val document: String = s"""
      mutation($$programId: ProgramId!, $$recipientEmail: String!) {
        createUserInvitation(input: {
          programId: $$programId,
          recipientEmail: $$recipientEmail,
          role: COI
        }) {
          key
          invitation {
            id
            status
            recipientEmail
          }
        }
      }
    """

    object Data:
      object CreateUserInvitation:
        type Invitation = CoIInvitation

  @GraphQL
  trait RevokeInvitationMutation extends GraphQLOperation[ObservationDB]:
    val document: String = s"""
      mutation($$id: UserInvitationId!) {
        revokeUserInvitation(input: { id: $$id }) {
          invitation {
            id
          }
        }
      }
    """
