// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import explore.model.GroupElement
import explore.model.Grouping
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*
import clue.annotation.GraphQL
// gql: import lucuma.schemas.decoders.given

object GroupQueriesGQL:

  @GraphQL
  object GroupElementsSubQuery
      extends GraphQLSubquery.Typed[ObservationDB, GroupElement]("GroupElement"):
    override val subquery: String = s"""
      {
        parentGroupId
        observation {
          id
        }
        group $GroupSubQuery
      }
    """

  @GraphQL
  object GroupSubQuery extends GraphQLSubquery.Typed[ObservationDB, Grouping]("Group"):
    override val subquery: String = s"""
      {
        id
        name
        minimumRequired
        elements {
          observation {
            id
          }
          group {
            id
          }
        }
      }
    """
