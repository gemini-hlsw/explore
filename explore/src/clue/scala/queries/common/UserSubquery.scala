// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import explore.model.User
import lucuma.schemas.ObservationDB

@GraphQL
object UserSubquery extends GraphQLSubquery.Typed[ObservationDB, User]("User"):
  override val subquery: String = s"""
    {
      id
      orcidId
      profile $UserProfileSubquery
    }
  """
