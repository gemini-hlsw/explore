// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import explore.model.ProgramUser
import lucuma.schemas.ObservationDB

@GraphQL
object ProgramUserSubquery extends GraphQLSubquery.Typed[ObservationDB, ProgramUser]("User"):
  override val subquery: String = """
    {
      id
      orcidId
      orcidGivenName
      orcidCreditName
      orcidFamilyName
      orcidEmail
    }
  """
