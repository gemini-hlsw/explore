// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import eu.timepit.refined.types.string.NonEmptyString
import lucuma.refined.*

enum WavelengthUnits(val label: NonEmptyString) {
  case Micrometers extends WavelengthUnits("μm".refined)
  case Nanometers  extends WavelengthUnits("nm".refined)
}
