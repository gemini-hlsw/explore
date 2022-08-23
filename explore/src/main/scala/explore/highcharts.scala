// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.highcharts

import gpp.highcharts.mod._
import japgolly.scalajs.react._
import react.common.style.Css

import scala.scalajs.js
import scala.scalajs.js.annotation._
import scala.scalajs.js.annotation._

extension (c: Chart_)
  inline def showLoadingCB = CallbackTo(c.showLoading())
  inline def showLoadingCB(str: String) = CallbackTo(c.showLoading(str))

type ChartSelection = ChartSelectionContextObject

extension (c: ChartOptions)
  inline def clazz(s: Css) = c.setClassName(s.htmlClass)

  inline def selectionCB(cb: ChartSelection => Callback) =
    c.setEvents(
      ChartEventsOptions().setSelection((_, s) => cb(s).runNow())
    )

@js.native
@JSImport("highcharts/es-modules/masters/modules/accessibility.src.js", JSImport.Default)
object HighchartsAccesibility extends js.Object
