// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString

trait Constants {
  val UnnamedTarget: NonEmptyString = "<UNNAMED>"
}

object Constants extends Constants
