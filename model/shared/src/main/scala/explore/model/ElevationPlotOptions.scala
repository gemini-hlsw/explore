// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import cats.derived.*
import explore.model.enums.PlotRange
import explore.model.enums.TimeDisplay
import lucuma.core.enums.Site
import monocle.Focus

final case class ElevationPlotOptions(
  site:  Site,
  range: PlotRange,
  time:  TimeDisplay
) derives Eq

object ElevationPlotOptions {
  val site  = Focus[ElevationPlotOptions](_.site)
  val range = Focus[ElevationPlotOptions](_.range)
  val time  = Focus[ElevationPlotOptions](_.time)

  val Default =
    ElevationPlotOptions(Site.GS, PlotRange.Night, TimeDisplay.Site)

}
