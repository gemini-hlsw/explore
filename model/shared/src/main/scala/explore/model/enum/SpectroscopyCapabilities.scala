// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enum

import lucuma.core.util.Enumerated

sealed abstract class SpectroscopyCapabilities(val label: String) extends Product with Serializable

object SpectroscopyCapabilities {
  case object NodAndShuffle extends SpectroscopyCapabilities("Nod & Shuffle")
  case object Polarimetry   extends SpectroscopyCapabilities("Polarimetry")
  case object Corongraphy   extends SpectroscopyCapabilities("Corongraphy")

  implicit val ConfigurationModeEnumerated: Enumerated[SpectroscopyCapabilities] =
    Enumerated.of(NodAndShuffle, Polarimetry, Corongraphy)
}
