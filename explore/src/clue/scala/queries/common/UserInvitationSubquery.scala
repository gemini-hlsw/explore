// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import explore.model.UserInvitation
import lucuma.schemas.ObservationDB

@GraphQL
object UserInvitationSubquery
    extends GraphQLSubquery.Typed[ObservationDB, UserInvitation]("UserInvitation"):
  override val subquery: String = """
    {
      id
      recipientEmail
      status
      email {
        status
      }
    }
  """
