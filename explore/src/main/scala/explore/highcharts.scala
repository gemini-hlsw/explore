// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.highcharts

import gpp.highcharts.mod._
import japgolly.scalajs.react._

extension (c: Chart_) inline def showLoadingCB = CallbackTo(c.showLoading())
