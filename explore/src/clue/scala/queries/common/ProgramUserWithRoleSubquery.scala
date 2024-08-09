// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import explore.model.ProgramUserWithRole
import lucuma.schemas.ObservationDB

@GraphQL
object ProgramUserWithRoleSubquery
    extends GraphQLSubquery.Typed[ObservationDB, ProgramUserWithRole]("ProgramUser"):
  override val subquery: String = s"""
    {
      user $ProgramUserSubquery
      partner {
        linkType
        ... on HasPartner {
          partner
        }
      }
      role
    }
  """
