// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.GraphQLSubquery
import clue.annotation.GraphQL
import explore.model.Group
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.TimeSpanSubquery

object GroupQueriesGQL:

  object GroupSubQuery extends GraphQLSubquery.Typed[ObservationDB, Group]("Group") {
    override val subquery: String = s"""
      {
        id
        name
        minimumRequired
        ordered
        system
        minimumInterval $TimeSpanSubquery
        maximumInterval $TimeSpanSubquery
        parentId 
        parentIndex
      }
    """
  }

  @GraphQL
  trait UpdateGroupsMutation extends GraphQLOperation[ObservationDB]:
    override val document = """
      mutation($input: UpdateGroupsInput!) {
        updateGroups(input: $input) {
          groups { id }
        }
      }
    """

  @GraphQL
  trait CreateGroupMutation extends GraphQLOperation[ObservationDB]:
    override val document = s"""
      mutation($$input: CreateGroupInput!) {
        createGroup(input: $$input) {
          group $GroupSubQuery
        }
      }"""
