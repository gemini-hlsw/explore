// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor.plots

import cats.Eq
import cats.derived.*
import lucuma.core.util.Display
import lucuma.core.util.Enumerated
import lucuma.react.highcharts.Chart

import scalajs.js

enum SeriesType(
  val name:      String,
  val yAxis:     Int,
  val threshold: Int,
  val data:      ObjectPlotData.SeriesData => js.Array[Chart.Data]
) derives Eq,
      Enumerated:
  val tag: String = name

  case Elevation
      extends SeriesType(
        "Elevation",
        0,
        90,
        _.targetAltitude.asInstanceOf[js.Array[Chart.Data]]
      )
  case ParallacticAngle
      extends SeriesType(
        "Parallactic Angle",
        1,
        -180,
        _.parallacticAngle.asInstanceOf[js.Array[Chart.Data]]
      )
  case SkyBrightness
      extends SeriesType(
        "Sky Brightness",
        2,
        22,
        _.skyBrightness.asInstanceOf[js.Array[Chart.Data]]
      )
  case LunarElevation
      extends SeriesType(
        "Lunar Elevation",
        0,
        90,
        _.moonAltitude.asInstanceOf[js.Array[Chart.Data]]
      )

object SeriesType:
  given Display[SeriesType] = Display.byShortName(_.name)
