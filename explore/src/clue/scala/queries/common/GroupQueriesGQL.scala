// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.GraphQLSubquery
import clue.annotation.GraphQL
import explore.model.GroupElement
import explore.model.Grouping
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.TimeSpanSubquery

object GroupQueriesGQL:

  @GraphQL
  object GroupElementsSubQuery
      extends GraphQLSubquery.Typed[ObservationDB, GroupElement]("GroupElement"):
    override val subquery: String = s"""
      {
        parentGroupId
        observation {
          id
          groupIndex
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
        parentId
        parentIndex
        ordered
        system
        minimumInterval $TimeSpanSubquery
        maximumInterval $TimeSpanSubquery
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

  @GraphQL
  trait UpdateGroupsMutation extends GraphQLOperation[ObservationDB]:
    override val document = """
      mutation($input: UpdateGroupsInput!) {
        updateGroups(input: $input) {
          groups {
            id
          }
        }
      }
    """

  @GraphQL
  trait CreateGroupMutation extends GraphQLOperation[ObservationDB]:
    override val document = s"""#graphql
      mutation($$input: CreateGroupInput!) {
        createGroup(input: $$input) {
          group $GroupSubQuery
        }
      }"""
