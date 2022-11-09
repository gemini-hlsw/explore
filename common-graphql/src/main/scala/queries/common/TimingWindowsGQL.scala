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
import java.time.ZonedDateTime
import io.circe.Encoder
import java.time.format.DateTimeFormatter
// gql: import queries.schemas.UserPreferencesDB.*

object TimingWindowsGQL:
  given Decoder[ZonedDateTime]     =
    Decoder.decodeZonedDateTimeWithFormatter(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
  given Encoder[ZonedDateTime]     =
    Encoder.encodeZonedDateTimeWithFormatter(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
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
