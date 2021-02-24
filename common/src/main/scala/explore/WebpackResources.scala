// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport
import scala.annotation.nowarn

object WebpackResources {
  @nowarn
  @js.native
  @JSImport("resources/images/ORCID-iD_icon-vector.svg", JSImport.Default)
  val OrcidLogo: String = js.native
}
