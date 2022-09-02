// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.highcharts

import gpp.highcharts.mod._
import japgolly.scalajs.react._
import react.common.style.Css

extension (c: Chart_)
  inline def showLoadingCB: Callback              = CallbackTo(c.showLoading())
  inline def showLoadingCB(str: String): Callback = CallbackTo(c.showLoading(str))
  inline def reflowCB: Callback                   = Callback(c.reflow())

type ChartSelection = ChartSelectionContextObject

extension (c: ChartOptions)
  inline def clazz(s: Css) = c.setClassName(s.htmlClass)

  inline def selectionCB(cb: ChartSelection => Callback) =
    c.setEvents(
      ChartEventsOptions().setSelection((_, s) => cb(s).runNow())
    )

def commonOptions: ChartOptions =
  ChartOptions()
    .setStyledMode(true)
    .setAlignTicks(false)
    .setZooming(ChartZoomingOptions().setType(OptionsTypeValue.xy))
    .setPanning(ChartPanningOptions().setEnabled(true))
    .setPanKey(OptionsPanKeyValue.shift)
