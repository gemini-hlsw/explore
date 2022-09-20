// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import lucuma.core.math.Coordinates
import lucuma.utils.NewType

// Tag to indicate the coordinates have been corrected for proper motion
object PMCoordinates extends NewType[Coordinates]
type PMCoordinates = PMCoordinates.Type
