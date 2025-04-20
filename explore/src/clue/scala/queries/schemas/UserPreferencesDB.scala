// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.schemas

import clue.annotation.GraphQLSchema
import explore.model.Constants
import io.circe.Decoder
import io.circe.Encoder
import lucuma.core.model.User
import lucuma.core.util.Enumerated
import lucuma.react.table.SortDirection

import java.time.ZonedDateTime

@GraphQLSchema
trait UserPreferencesDB:
  given Decoder[ZonedDateTime] =
    Decoder.decodeZonedDateTimeWithFormatter(Constants.IsoUTCFormatter)

  given Encoder[ZonedDateTime] =
    Encoder.encodeZonedDateTimeWithFormatter(Constants.IsoUTCFormatter)

  type Timestamptz = java.time.ZonedDateTime
  type Timestamp   = java.time.ZonedDateTime

  given Enumerated[SortDirection] =
    Enumerated.from(SortDirection.Ascending, SortDirection.Descending).withTag {
      case SortDirection.Ascending  => "ASC"
      case SortDirection.Descending => "DESC"
    }

  object Scalars:
    type UserId        = User.Id
    type ResizableArea = String
    type Bigint        = Long

  object Enums:
    type ItcChartTypeEnum             = lucuma.itc.GraphType
    type LucumaTableIdsEnum           = explore.model.enums.TableId
    type LucumaSortDirectionEnum      = SortDirection
    type LucumaGridLayoutIdEnum       = explore.model.enums.GridLayoutSection
    type LucumaGridBreakpointNameEnum = explore.model.enums.GridBreakpointName
    type ExplorePlotRangeEnum         = explore.model.enums.PlotRange
    type ExplorePlotTimeEnum          = explore.model.enums.TimeDisplay
    type LucumaWavelengthUnitsEnum    = explore.model.enums.WavelengthUnits
