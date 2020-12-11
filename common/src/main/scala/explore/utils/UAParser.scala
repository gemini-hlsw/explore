// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import scala.scalajs.js
import scala.scalajs.js.annotation._

@js.native
trait Browser extends js.Object {
  val name: String    = js.native
  val version: String = js.native
}

@js.native
@JSImport("ua-parser-js", JSImport.Namespace)
class UAParser(val ua: String) extends js.Object {
  def getBrowser(): Browser = js.native
}
