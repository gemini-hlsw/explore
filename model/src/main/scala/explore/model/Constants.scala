// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString

trait Constants {
  val UnnamedTarget: NonEmptyString   = "<UNNAMED>"
  val UnnamedAsterism: NonEmptyString = "<UNNAMED>"
  val TwoPanelCutoff                  = 550.0
  val InitialTreeWidth                = 300.0
  val MinLeftPanelWidth               = 270.0
  val GridRowHeight                   = 36
}

object Constants extends Constants
