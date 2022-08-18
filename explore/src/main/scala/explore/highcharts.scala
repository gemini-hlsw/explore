// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.highcharts

import gpp.highcharts.mod._
import japgolly.scalajs.react._

import scala.scalajs.js
import scala.scalajs.js.annotation._

extension (c: Chart_) inline def showLoadingCB = CallbackTo(c.showLoading())

@js.native
@JSImport("highcharts/es-modules/masters/modules/accessibility.src.js", JSImport.Default)
object HighchartsAccesibility extends js.Object
