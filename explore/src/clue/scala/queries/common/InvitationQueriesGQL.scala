// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB

object InvitationQueriesGQL:
  @GraphQL
  trait CreateInviteMutation extends GraphQLOperation[ObservationDB]:
    val document: String = s"""
      mutation inviteToProgram($$programId: ProgramId!, $$recipientEmail: String!) {
        createUserInvitation(input: { programId: $$programId, recipientEmail: $$recipientEmail, role: COI }) {
          key
          invitation {
            id
            status
            issuer {
              id
            }
            redeemer {
              id
            }
            program {
              id
            }
            role
            supportType
            supportPartner
          }
        }
      }
    """
