// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import explore.model.ProgramInfo
import lucuma.schemas.ObservationDB

@GraphQL
object ProgramInfoSubquery extends GraphQLSubquery.Typed[ObservationDB, ProgramInfo]("Program"):
  override val subquery: String = s"""
    {
      id
      name
      pi $ProgramUserSubquery,
      users {
        user {
          id
        }
        role
      }
      type
      reference $ProgramReferenceSubquery
      proposal {
        reference {
          label
        }
      }
      proposalStatus
      existence
    }
  """
