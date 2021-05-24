// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enum

import lucuma.core.util.Enumerated

sealed abstract class ImagingCapabilities(val label: String) extends Product with Serializable

object ImagingCapabilities {
  case object Polarimetry        extends ImagingCapabilities("Polarimetry")
  case object Speckle            extends ImagingCapabilities("Speckle")
  case object Corongraphy        extends ImagingCapabilities("Corongraphy")
  case object LowOverheadReadout extends ImagingCapabilities("Low Overhead Readout")

  implicit val ConfigurationModeEnumerated: Enumerated[ImagingCapabilities] =
    Enumerated.of(Polarimetry, Speckle, Corongraphy, LowOverheadReadout)
}
