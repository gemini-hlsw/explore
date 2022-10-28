// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.schemas

import clue.annotation.GraphQLSchema
import explore.model.enums
import lucuma.core.model.User
import lucuma.core.util.Enumerated
import lucuma.react.table.SortDirection

@GraphQLSchema
trait UserPreferencesDB:
  type Timestamptz = java.time.ZonedDateTime

  given Enumerated[SortDirection] =
    Enumerated.from(SortDirection.Ascending, SortDirection.Descending).withTag {
      case SortDirection.Ascending  => "ASC"
      case SortDirection.Descending => "DESC"
    }

  object Scalars:
    type UserId             = User.Id
    type ResizableArea      = String
    type BreakpointName     = String
    type Site               = lucuma.core.enums.Site
    type ElevationPlotRange = explore.model.enums.PlotRange
    type ElevationPlotTime  = explore.model.enums.TimeDisplay
    type Bigint             = Long

  object Enums:
    type ItcChartType            = enums.ItcChartType
    type LucumaTableIdsEnum      = explore.model.enums.TableId
    type LucumaSortDirectionEnum = SortDirection
    type LucumaGridLayoutIdEnum  = explore.model.enums.GridLayoutSection

