// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.GraphQLSubquery
import clue.annotation.GraphQL
import explore.model.Group
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.TimeSpanSubquery
// gql: import io.circe.refined.given

object GroupQueriesGQL:

  // object GroupElementsSubQuery
  //     extends GraphQLSubquery.Typed[ObservationDB, GroupElement]("GroupElement"):
  //   override val subquery: String = s"""
  //     {
  //       parentGroupId # Only used for identifying root group
  //       parentIndex   # Only used if element is in root group
  //       observation { id }
  //       group $GroupSubQuery
  //     }
  //   """

  object GroupSubQuery extends GraphQLSubquery.Typed[ObservationDB, Group]("Group"):
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

  // object GroupElementSubQuery
  //     extends GraphQLSubquery.Typed[ObservationDB, GroupWithChildren.Child]("Group"):
  //   override val subquery: String = s"""
  //     {
  //       observation {
  //         id
  //         groupIndex
  //       }
  //       group {
  //         id
  //         parentIndex
  //       }
  //     }
  //   """

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
          meta:group { parentIndex }
        }
      }"""
