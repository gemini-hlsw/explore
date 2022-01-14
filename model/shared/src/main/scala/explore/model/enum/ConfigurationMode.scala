// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enum

import lucuma.core.util.Enumerated

sealed abstract class ConfigurationMode(val label: String) extends Product with Serializable

object ConfigurationMode {
  case object Imaging      extends ConfigurationMode("Imaging")
  case object Spectroscopy extends ConfigurationMode("Spectroscopy")

  implicit val ConfigurationModeEnumerated: Enumerated[ConfigurationMode] =
    Enumerated.of(Imaging, Spectroscopy)
}
