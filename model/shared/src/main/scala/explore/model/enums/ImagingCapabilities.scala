// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.util.Enumerated

enum ImagingCapabilities(val label: String):
  case Polarimetry        extends ImagingCapabilities("Polarimetry")
  case Speckle            extends ImagingCapabilities("Speckle")
  case Corongraphy        extends ImagingCapabilities("Corongraphy")
  case LowOverheadReadout extends ImagingCapabilities("Low Overhead Readout")

object ImagingCapabilities:
  given Enumerated[ImagingCapabilities] =
    Enumerated.from(Polarimetry, Speckle, Corongraphy, LowOverheadReadout).withTag(_.label)
