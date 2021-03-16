// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import lucuma.core.math.Angle

trait Constants {
  val TwoPanelCutoff    = 576.0
  val InitialTreeWidth  = 300.0
  val MinLeftPanelWidth = 270.0
  val GridRowHeight     = 36
  val InitialFov: Angle = Angle.fromDoubleDegrees(0.25)
}

object Constants extends Constants
