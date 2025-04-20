// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.schemas

import clue.annotation.GraphQLSchema

@GraphQLSchema
trait SSO:
  object Scalars:
    type UserId   = lucuma.core.model.User.Id
    type RoleId   = lucuma.core.model.StandardRole.Id
    type ApiKeyId = String

  object Enums:
    type RoleType = lucuma.core.enums.RoleType
