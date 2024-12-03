// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
import explore.model.UserInvitation
import explore.model.ProgramInvitation

object InvitationQueriesGQL:
  @GraphQL
  trait CreateInviteMutation extends GraphQLOperation[ObservationDB]:
    val document: String = s"""
      mutation($$programId: ProgramId!, $$recipientEmail: String!, $$role: ProgramUserRole!) {
        createUserInvitation(input: {
          programId: $$programId,
          recipientEmail: $$recipientEmail,
          role: $$role
        }) {
          key
          invitation {
            id
            status
            recipientEmail
            role
            email {
              status
            }
          }
        }
      }
    """

    object Data:
      object CreateUserInvitation:
        type Invitation = UserInvitation

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

  @GraphQL
  trait RedeemInvitationMutation extends GraphQLOperation[ObservationDB]:
    val document: String = s"""
      mutation($$key: UserInvitationKey!) {
        redeemUserInvitation(input: { key: $$key }) {
          invitation {
            issuer {
              profile {
                givenName
                familyName
              }
            }
            program {
              id
            }
          }
        }
      }
    """

    object Data:
      object RedeemUserInvitation:
        type Invitation = ProgramInvitation
