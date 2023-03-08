// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model.Constants
import explore.model.TimingWindowEntry
import queries.schemas.UserPreferencesDB

// gql: import queries.schemas.UserPreferencesDB.*
// gql: import queries.schemas.UserPreferencesDB.given

object TimingWindowsGQL:

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

  @GraphQL
  trait TimingWindowSubscription extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      subscription TimingWindowsSuscription {
        tmpTimingWindows {
          id
        }
      }"""
  }

  @GraphQL
  trait InsertTimingWindow extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      mutation insertTimingWindow($startsOn: timestamptz = "") {
        insertTmpTimingWindowsOne(object: {startsOn: $startsOn, forever: true}) {
          id
        }
      }"""
  }

  @GraphQL
  trait DeleteTimingWindow extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      mutation deleteTimingWindow($id: Int = 10) {
        deleteTmpTimingWindowsByPk(id: $id) {
          id
        }
      }
    """
  }

  @GraphQL
  trait UpdateTimingWindow extends GraphQLOperation[UserPreferencesDB] {
    val document = """
      mutation updateTimingWindow($id: Int = 10, $_set: TmpTimingWindowsSetInput = {}) {
        updateTmpTimingWindowsByPk(pk_columns: {id: $id}, _set: $_set) {
          id
        }
      }
    """
  }
