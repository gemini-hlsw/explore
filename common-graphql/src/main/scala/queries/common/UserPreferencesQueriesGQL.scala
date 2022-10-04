// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import queries.schemas.UserPreferencesDB

object UserPreferencesQueriesGQL {

  /**
   * Query to create a user, this is called when the app is started. If the user exists the error is
   * ignored
   */
  @GraphQL
  trait UserInsertMutation extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      mutation insertUser($id: String) {
        insert_lucumaUser_one(
          object: {
            userId: $id
          },
          on_conflict: {
            update_columns: [],
            constraint: lucuma_user_pkey
          }
        ) {
          userId
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
      mutation updateAreaWidth($item: lucumaResizableWidth_insert_input!) {
        insert_lucumaResizableWidth_one(
          object: $item,
          on_conflict: {
            constraint: explore_resizable_width_pkey,
            update_columns: width
          }
        ) {
          userId
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
      query areaWidth($userId: String!, $section: resizable_area!) {
        lucumaResizableWidth_by_pk(
          section: $section,
          userId: $userId
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
  trait UserGridLayoutQuery extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      query
        obsTabPreferences($userId: String!, $criteria: lucumaGridLayoutPositions_bool_exp!, $section: resizable_area!) {
          lucumaGridLayoutPositions(where: $criteria) {
            breakpointName
            height
            width
            x
            y
            tile
          }
          lucumaResizableWidth_by_pk(
            section: $section,
            userId: $userId
          ) {
            width
          }
        }
    """
  }

  @GraphQL
  trait UserGridLayoutUpsert extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      mutation insertLayoutPositions($objects: [lucumaGridLayoutPositions_insert_input!]! = {}) {
        insert_lucumaGridLayoutPositions(objects: $objects, on_conflict: {
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
      query targetPreferences($userId: String! = "", $targetId: String! = "") {
        lucumaTargetPreferences_by_pk(targetId: $targetId, userId: $userId) {
          fovRA
          fovDec
          viewOffsetP
          viewOffsetQ
          agsCandidates
          agsOverlay
          fullScreen
        }
        lucumaUserPreferences_by_pk(userId: $userId) {
          aladinMouseScroll
        }
      }
    """
  }

  @GraphQL
  trait UserElevationPlotPreferencesQuery extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      query plotPreferences($userId: String! = "") {
        lucumaUserPreferences_by_pk(userId: $userId) {
          elevationPlotRange
          elevationPlotTime
        }
      }
    """
  }

  @GraphQL
  trait UserPreferencesAladinUpdate extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      mutation userPreferencesUpsert($userId: String = "", $aladinMouseScroll: Boolean = false) {
        insert_lucumaUserPreferences_one(
          object: {
            userId: $userId,
            aladinMouseScroll: $aladinMouseScroll
          },
          on_conflict: {
            constraint: lucuma_user_preferences_pkey,
            update_columns: aladinMouseScroll
          }
        ) {
          userId
        }
      }
    """
  }

  @GraphQL
  trait UserPreferencesElevPlotUpdate extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      mutation userPreferencesUpsert($userId: String = "", $elevationPlotRange: elevation_plot_range = "", $elevationPlotTime: elevation_plot_time = "") {
        insert_lucumaUserPreferences_one(
          object: {
            userId: $userId,
            elevationPlotRange: $elevationPlotRange,
            elevationPlotTime: $elevationPlotTime
          },
          on_conflict: {
            constraint: lucuma_user_preferences_pkey,
            update_columns: [
              elevationPlotRange,
              elevationPlotTime
            ]
          }
        ) {
          userId
        }
      }
    """
  }

  @GraphQL
  trait UserTargetPreferencesUpsert extends GraphQLOperation[UserPreferencesDB] {
    val document =
      """mutation targetPreferencesUpsert($objects: lucumaTarget_insert_input! = {}) {
        insert_lucumaTarget(objects: [$objects], on_conflict: {constraint: lucuma_target_pkey, update_columns: targetId}) {
          affected_rows
        }
      }"""
  }

  @GraphQL
  trait UserTargetViewOffsetUpdate extends GraphQLOperation[UserPreferencesDB] {
    val document =
      """ mutation updateTargetViewOffsetv($userId: String!, $targetId: String!, $viewOffsetP: bigint!, $viewOffsetQ: bigint!) {
        update_lucumaTargetPreferences_by_pk(
          pk_columns: {
            userId: $userId,
            targetId: $targetId
          }
          _set: {
            viewOffsetP: $viewOffsetP,
            viewOffsetQ: $viewOffsetQ
          }
        ) {
          targetId
        }
      }"""
  }

  @GraphQL
  trait UserTargetPreferencesFovUpdate extends GraphQLOperation[UserPreferencesDB] {
    val document =
      """ mutation updateTargetFov($userId: String!, $targetId: String!, $fovRA: bigint!, $fovDec: bigint!) {
        update_lucumaTargetPreferences_by_pk(
          pk_columns: {
            userId: $userId,
            targetId: $targetId
          }
          _set: {
            fovRA: $fovRA,
            fovDec: $fovDecc
          }
        ) {
          targetId
        }
      }"""
  }
  @GraphQL
  trait ItcPlotPreferencesQuery        extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      query itcPlotPreferences($userId: String! = "", $observationId: String! = "") {
        lucumaItcPlotPreferences_by_pk(observationId: $observationId, userId: $userId) {
          chartType
          detailsOpen
        }
      }
    """
  }

  @GraphQL
  trait ItcPlotObservationUpsert extends GraphQLOperation[UserPreferencesDB] {
    val document =
      """mutation observationPreferencesUpsert($objects: lucumaObservation_insert_input! = {}) {
        insert_lucumaObservation(objects: [$objects], on_conflict: {constraint: lucuma_observation_pkey, update_columns: observationId}) {
          affected_rows
        }
      }"""
  }

}
