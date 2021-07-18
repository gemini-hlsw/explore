// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enum

import lucuma.core.util.Enumerated

// TODO Move to lucuma core
sealed abstract class ScienceMode extends Product with Serializable

object ScienceMode {
  case object Imaging      extends ScienceMode
  case object Spectroscopy extends ScienceMode

  implicit val ScienceModeEnumerated: Enumerated[ScienceMode] =
    Enumerated.of(Imaging, Spectroscopy)
}
