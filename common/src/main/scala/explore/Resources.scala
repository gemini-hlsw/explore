// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import scala.annotation.nowarn
import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

object Resources {
  @nowarn
  @js.native
  @JSImport("/images/ORCID-iD_icon-vector.svg", JSImport.Default)
  val OrcidLogo: String = js.native
}
