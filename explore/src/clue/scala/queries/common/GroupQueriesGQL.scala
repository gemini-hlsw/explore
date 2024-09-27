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
        parentGroupId # Only used for identifying root group
        parentIndex   # Only used if element is in root group
        observation { id }
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
        ordered
        system
        minimumInterval $TimeSpanSubquery
        maximumInterval $TimeSpanSubquery
        elements {
          observation { 
            id
            groupIndex
          }
          group {
            id
            parentIndex
          }
        }
      }
    """

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
