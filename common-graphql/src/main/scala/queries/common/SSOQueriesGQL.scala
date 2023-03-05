// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import queries.schemas.SSO

object SSOQueriesGQL:

  @GraphQL
  trait UserQuery extends GraphQLOperation[SSO]:
    val document =
      """
      query {
        role {
          type
        }
        user {
          id
          givenName
          familyName
          apiKeys {
            id
            role {
              type
            }
          }
        }
      }
    """

    object Data:
      object User:
        type ApiKeys = explore.model.ApiKey

  @GraphQL
  trait NewApiKey extends GraphQLOperation[SSO]:
    val document =
      """
        mutation($roleId: RoleId!) {
          createApiKey(role: $roleId)
        }
    """
