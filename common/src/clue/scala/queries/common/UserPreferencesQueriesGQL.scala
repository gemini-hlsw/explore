// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import queries.schemas.UserPreferencesDB
import explore.model.GlobalPreferences
// gql: import queries.schemas.UserPreferencesDB.given

object UserPreferencesQueriesGQL {

  /**
   * Query to create a user, this is called when the app is started. If the user exists the error is
   * ignored
   */
  @GraphQL
  trait UserInsertMutation extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      mutation insertUser($id: String) {
        insertLucumaUserOne(
          object: {
            userId: $id
          },
          onConflict: {
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
   * Read the grid layout for a given section
   */
  @GraphQL
  trait FinderChartTransformationQuery extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      query finderCharById($attachmentId: String!, $observationId: String!) {
        exploreFinderChartByPk(attachmentId: $attachmentId, observationId: $observationId) {
          flipX
          flipY
          rotate
          scaleX
          scaleY
          inverted
        }
      }
    """
  }

  @GraphQL
  trait FinderChartUpsert extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      mutation finderChartUpsert($object: ExploreFinderChartInsertInput!) {
        insertExploreFinderChartOne(onConflict: {constraint: exploreFinderChart_pkey, update_columns: [flipX, flipY, rotate, scaleX, scaleY, inverted]}, object: $object) {
          attachmentId
        }
      }
      """
  }

  /**
   * Read the grid layout or a given section
   */
  @GraphQL
  trait UserGridLayoutQuery extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      query tabPositions($userId: String!){
        lucumaGridLayoutPositions(where: {userId: {_eq: $userId}}) {
          breakpointName
          section
          height
          width
          x
          y
          tile
        }
      }
    """
  }

  @GraphQL
  trait UserGridLayoutUpdates extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      subscription gridLayoutPositions($userId: String!) {
        lucumaGridLayoutPositions(where: {userId: {_eq: $userId}}) {
          breakpointName
          section
          height
          width
          x
          y
          tile
        }
      }
    """
  }

  @GraphQL
  trait UserGridLayoutUpsert extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      mutation insertLayoutPositions($objects: [LucumaGridLayoutPositionsInsertInput!]! = {}) {
        insertLucumaGridLayoutPositions(objects: $objects, onConflict: {
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
        exploreTargetPreferencesByPk(targetId: $targetId, userId: $userId) {
          fovRA
          fovDec
          viewOffsetP
          viewOffsetQ
          saturation
          brightness
        }
      }
    """
  }

  @GraphQL
  trait UserPreferencesQuery extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      query userPreferences($userId: String!) {
        lucumaUserPreferencesByPk(userId: $userId) {
          aladinMouseScroll
          showCatalog
          fullScreen
          agsOverlay
          scienceOffsets
          acquisitionOffsets
          elevationPlotRange
          elevationPlotTime
          elevationPlotScheduling
          itcChartType
          itcDetailsOpen
        }
      }
    """

    object Data:
      type LucumaUserPreferencesByPk = GlobalPreferences
  }

  @GraphQL
  trait UserPreferencesUpdates extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      subscription userPreferences($userId: String!) {
        lucumaUserPreferencesByPk(userId: $userId) {
          aladinMouseScroll
          showCatalog
          fullScreen
          agsOverlay
          scienceOffsets
          acquisitionOffsets
          elevationPlotRange
          elevationPlotTime
          elevationPlotScheduling
          itcChartType
          itcDetailsOpen
        }
      }
    """

    object Data:
      type LucumaUserPreferencesByPk = GlobalPreferences
  }

  @GraphQL
  trait UserPreferencesAladinUpdate extends GraphQLOperation[UserPreferencesDB] {
    val document = """
    mutation aladinUserPreferences($objects: LucumaUserPreferencesInsertInput! = {}) {
      insertLucumaUserPreferences(objects: [$objects], onConflict: {constraint: lucuma_user_preferences_pkey, update_columns: [fullScreen, showCatalog, aladinMouseScroll, agsOverlay, scienceOffsets, acquisitionOffsets]}) {
        affected_rows
      }
    }"""
  }

  @GraphQL
  trait UserPreferencesElevPlotUpdate extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      mutation userPreferencesUpsert($userId: String = "", $elevationPlotRange: ExplorePlotRangeEnum!, $elevationPlotTime: ExplorePlotTimeEnum!, $elevationPlotScheduling: Boolean!) {
        insertLucumaUserPreferencesOne(
          object: {
            userId: $userId,
            elevationPlotRange: $elevationPlotRange,
            elevationPlotTime: $elevationPlotTime,
            elevationPlotScheduling: $elevationPlotScheduling
          },
          onConflict: {
            constraint: lucuma_user_preferences_pkey,
            update_columns: [
              elevationPlotRange,
              elevationPlotTime,
              elevationPlotScheduling
            ]
          }
        ) {
          userId
        }
      }
    """
  }

  @GraphQL
  trait UserPreferencesItcPlotUpdate extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      mutation userPreferencesUpsert($userId: String = "", $itcChartType: ItcChartTypeEnum!, $itcDetailsOpen: Boolean!) {
        insertLucumaUserPreferencesOne(
          object: {
            userId: $userId,
            itcChartType: $itcChartType
            itcDetailsOpen: $itcDetailsOpen
          },
          onConflict: {
            constraint: lucuma_user_preferences_pkey,
            update_columns: [
              itcChartType,
              itcDetailsOpen
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
      """mutation targetPreferencesUpsert($objects: LucumaTargetInsertInput! = {}) {
        insertLucumaTarget(objects: [$objects], onConflict: {constraint: lucuma_target_pkey, update_columns: targetId}) {
          affected_rows
        }
      }"""
  }

  @GraphQL
  trait UserTargetPreferencesFovUpdate extends GraphQLOperation[UserPreferencesDB] {
    val document =
      """ mutation updateTargetFov($userId: String!, $targetId: String!, $fovRA: bigint!, $fovDec: bigint!) {
        updateExploreTargetPreferencesByPk(
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
  trait TableColumnPreferencesQuery extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      query tableColumnPreferences($tableId: LucumaTableIdsEnum, $userId: String) {
        lucumaTableColumnPreferences(where: {tableId: {_eq: $tableId}, userId: {_eq: $userId}}) {
          visible
          columnId
          sorting
          sortingOrder
        }
      }"""
  }

  @GraphQL
  trait TableColumnPreferencesUpsert extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      mutation tableColumnPreferencesUpsert($objects: [LucumaTableColumnPreferencesInsertInput!]!) {
        insertLucumaTableColumnPreferences(
          objects: $objects,
          onConflict: {
            constraint: lucumaTableColumnPreferences_pkey,
            update_columns: [visible, sorting, sortingOrder]
          }) {
          affected_rows
        }
      }"""
  }
}
