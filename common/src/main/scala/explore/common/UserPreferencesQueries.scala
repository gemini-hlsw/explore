// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.target

import clue.GraphQLOperation
import clue.macros.GraphQL
import explore.GraphQLSchemas.UserPreferencesDB

object UserPreferencesQueries {

  @GraphQL
  object UserInsertMutation extends GraphQLOperation[UserPreferencesDB] {
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

  @GraphQL
  object UserWidthsCreation extends GraphQLOperation[UserPreferencesDB] {
    val document = """
        mutation initial_area_width($userId: String, $width: Int) {
          insert_explore_resizable_width(
            objects: [
              {section: "observations_tree", user_id: $userId, width: $width},
              {section: "targets_tree", user_id: $userId, width: $width}
            ], on_conflict: {
              constraint: explore_resizable_width_pkey,
              update_columns: []
            }) {
            returning {
              user_id
            }
          }
        }
      }
   """
  }
}
