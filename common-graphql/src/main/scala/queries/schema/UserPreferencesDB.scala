// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.schemas

import clue.annotation.GraphQLSchema
import lucuma.core.model.User

@GraphQLSchema
trait UserPreferencesDB {
  object Scalars {
    type UserId             = User.Id
    type ResizableArea      = String
    type BreakpointName     = String
    type GridLayoutArea     = String
    type Site               = lucuma.core.enum.Site
    type ElevationPlotTime  = String
    type ElevationPlotRange = String
    type Bigint             = Long
  }
}
