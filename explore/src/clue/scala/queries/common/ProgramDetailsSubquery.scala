// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import explore.model.ProgramDetails
import lucuma.schemas.ObservationDB

@GraphQL
object ProgramDetailsSubquery
    extends GraphQLSubquery.Typed[ObservationDB, ProgramDetails]("Program"):
  override val subquery: String = s"""
    {
      type
      pi $ProgramUserWithRoleSubquery
      proposal $ProposalSubquery
      proposalStatus
      users $ProgramUserWithRoleSubquery
      reference $ProgramReferenceSubquery
      userInvitations $ProgramInvitationsSubquery
      allocations $AllocationSubquery
      goa {
        proprietaryMonths
      }
    }
  """
