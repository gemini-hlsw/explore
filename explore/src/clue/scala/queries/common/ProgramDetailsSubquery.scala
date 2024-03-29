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
      proposal $ProposalSubquery
      proposalStatus
      pi $ProgramUserSubquery
      users $ProgramUserWithRoleSubquery
      timeEstimateRange $ProgramTimeRangeSubquery
      timeCharge $ProgramTimeSubquery
    }
  """
