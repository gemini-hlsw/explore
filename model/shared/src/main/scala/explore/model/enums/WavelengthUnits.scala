// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.util.Enumerated

enum WavelengthUnits(val tag: String, val symbol: String) derives Enumerated {
  case Micrometers extends WavelengthUnits("micrometers", "μm")
  case Nanometers  extends WavelengthUnits("nanometers", "nm")
}
