// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import lucuma.core.model.UserProfile
import lucuma.schemas.ObservationDB
import lucuma.sso.client.codec.userProfile.given

@GraphQL
object UserProfileSubquery extends GraphQLSubquery.Typed[ObservationDB, UserProfile]("UserProfile"):
  override val subquery: String = """
    {
      givenName
      creditName
      familyName
      email
    }
  """
