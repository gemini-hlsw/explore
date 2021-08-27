// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.schemas.UserPreferencesDB
// gql: import lucuma.ui.reusability._

object UserPreferencesQueriesGQL {

  /**
   * Query to create a user, this is called when the app is started. If the user exists the error is
   * ignored
   */
  @GraphQL
  trait UserInsertMutation extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      mutation insert_user($id: String) {
        insert_lucuma_user_one(
          object: {
            user_id: $id
          },
          on_conflict: {
            update_columns: [],
            constraint: lucuma_user_pkey
          }
        ) {
          user_id
          }
        }
    """
  }

  /**
   * Update the width of a given area/user
   */
  @GraphQL
  trait UserWidthsCreation extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      mutation update_area_width($item: explore_resizable_width_insert_input!) {
        insert_explore_resizable_width_one(
          object: $item,
          on_conflict: {
            constraint: explore_resizable_width_pkey,
            update_columns: width
          }
        ) {
          user_id
          }
        }
   """
  }

  /**
   * Read the stored width of an area
   */
  @GraphQL
  trait UserAreaWidths extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      query area_width($user_id: String!, $section: resizable_area!) {
        explore_resizable_width_by_pk(
          section: $section,
          user_id: $user_id
        ) {
          width
        }
      }
    """
  }

  /**
   * Read the grid layout for a given section
   */
  @GraphQL
  trait ObsTabPreferencesQuery extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      query
        obs_tab_preferences($user_id: String!, $criteria: grid_layout_positions_bool_exp!, $section: resizable_area!) {
          grid_layout_positions(where: $criteria) {
            breakpoint_name
            height
            width
            x
            y
            tile
          }
          explore_resizable_width_by_pk(
            section: $section,
            user_id: $user_id
          ) {
            width
          }
        }
    """
  }

  @GraphQL
  trait UserGridLayoutUpsert extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      mutation insert_layout_positions($objects: [grid_layout_positions_insert_input!]! = {}) {
        insert_grid_layout_positions(objects: $objects, on_conflict: {
          constraint: grid_layout_positions_pkey,
          update_columns: [width, height, x, y]
        }) {
          affected_rows
        }
      }"""
  }

  @GraphQL
  trait UserTargetPreferencesQuery extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      query target_preferences($user_id: String! = "", $targetId: String! = "") {
        lucuma_target_preferences_by_pk(targetId: $targetId, user_id: $user_id) {
          fov
        }
      }
    """
  }

  @GraphQL
  trait UserTargetPreferencesUpsert extends GraphQLOperation[UserPreferencesDB] {
    val document =
      """mutation target_preferences_upsert($objects: lucuma_target_insert_input! = {}) {
        insert_lucuma_target(objects: [$objects], on_conflict: {constraint: lucuma_target_pkey, update_columns: targetId}) {
          affected_rows
        }
      }"""
  }
}
