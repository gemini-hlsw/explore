// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.highcharts

import japgolly.scalajs.react.*
import lucuma.react.common.style.Css
import lucuma.typed.highcharts.mod.*

extension (c: Chart_)
  inline def showLoadingCB: Callback = CallbackTo(c.showLoading())
  inline def showLoadingCB(str: String): Callback = CallbackTo(c.showLoading(str))
  inline def reflowCB: Callback = Callback(c.reflow())

extension (c: ChartOptions)
  inline def clazz(s: Css) = c.setClassName(s.htmlClass)

  inline def selectionCB(cb: SelectEventObject => Callback) =
    c.setEvents(
      ChartEventsOptions().setSelection((_, s) => cb(s).runNow())
    )

extension (c: XAxisPlotLinesOptions) inline def clazz(s: Css) = c.setClassName(s.htmlClass)

def commonOptions: ChartOptions =
  ChartOptions()
    .setStyledMode(true)
    .setAlignTicks(false)
    .setZooming(ChartZoomingOptions().setType(OptionsChartZoomingTypeValue.xy))
    .setPanning(ChartPanningOptions().setEnabled(true))
    .setPanKey(OptionsPanKeyValue.shift)
