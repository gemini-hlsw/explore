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
      mutation finderChartUpsert($observationId: String!, $exploreFinderCharts: ExploreFinderChartInsertInput!) {
        insertLucumaObservationOne(
          object: {
            observationId: $observationId,
            exploreFinderCharts: {
              data: [$exploreFinderCharts],
              onConflict: {
                constraint: exploreFinderChart_pkey,
                update_columns: [flipX, flipY, rotate, scaleX, scaleY, inverted]
              }
            }
          },
          onConflict: {
            constraint: lucuma_observation_pkey,
            update_columns: observationId
          }
        ) {
            observationId
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
  trait AsterismPreferencesQuery extends GraphQLOperation[UserPreferencesDB] {
    val document = """
    query asterismPreferences($userId: String!, $targetIds: [String!] = "") {
      exploreAsterismPreferences(where: {_and: {userId: {_eq: $userId}, lucumaAsterisms: {targetId: {_in: $targetIds}}}}) {
        id
        brightness
        saturation
        fovRA
        fovDec
        viewOffsetP
        viewOffsetQ
        lucumaAsterisms {
          targetId
        }
      }
    }
    """
  }

  @GraphQL
  trait AsterismUpsert extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      mutation asterismUpsert($object: ExploreAsterismPreferencesInsertInput!, $updateColumns: [ExploreAsterismPreferencesUpdateColumn!]) {
        insertExploreAsterismPreferencesOne(object: $object, onConflict: {constraint: exploreAsterismPreferences_pkey, update_columns: $updateColumns}) {
          id
        }
      }"""
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
    mutation aladinUserPreferences($objects: LucumaUserPreferencesInsertInput! = {}, $update_columns: [LucumaUserPreferencesUpdateColumn!]) {
      insertLucumaUserPreferencesOne(object: $objects, onConflict: {constraint: lucuma_user_preferences_pkey, update_columns: $update_columns}) {
        userId
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
