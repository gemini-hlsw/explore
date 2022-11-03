// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model.TimingWindowEntry
import io.circe.Decoder
import io.circe.generic.semiauto.*
import lucuma.schemas.decoders.*
import queries.schemas.UserPreferencesDB
// gql: import queries.schemas.UserPreferencesDB.*

object TimingWindowsGQL:
  given Decoder[TimingWindowEntry] = deriveDecoder

  /**
   * Read the grid layout for a given section
   */
  @GraphQL
  trait TimingWindowsQuery extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      query queryTimingWindwons {
        tmpTimingWindows(orderBy: {id: ASC}) {
          startsOn
          repeatPeriod
          repeatForever
          repeatTimes
          remainOpenFor
          id
          forever
          closeOn
        }
      }
    """

    object Data {
      type TmpTimingWindows = TimingWindowEntry
    }
  }

  // @GraphQL
  // trait UserGridLayoutUpsert extends GraphQLOperation[UserPreferencesDB] {
  //   val document = """
  //     mutation insertLayoutPositions($objects: [LucumaGridLayoutPositionsInsertInput!]! = {}) {
  //       insertLucumaGridLayoutPositions(objects: $objects, onConflict: {
  //         constraint: grid_layout_positions_pkey,
  //         update_columns: [width, height, x, y]
  //       }) {
  //         affected_rows
  //       }
  //     }"""
  // }
