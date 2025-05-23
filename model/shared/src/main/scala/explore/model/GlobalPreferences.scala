// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import explore.model.enums.PlotRange
import explore.model.enums.TimeDisplay
import explore.model.enums.Visible
import explore.model.enums.WavelengthUnits
import explore.model.itc.PlotDetails
import io.circe.Decoder
import lucuma.core.util.NewBoolean
import lucuma.itc.GraphType
import monocle.Focus

object AladinMouseScroll extends NewBoolean:
  inline def Allowed = True; inline def Disabled = False
type AladinMouseScroll = AladinMouseScroll.Type

object ElevationPlotScheduling extends NewBoolean { val On = True; val Off = False }
type ElevationPlotScheduling = ElevationPlotScheduling.Type

case class GlobalPreferences(
  aladinMouseScroll:                    AladinMouseScroll,
  fullScreen:                           AladinFullScreen,
  showCatalog:                          Visible,
  agsOverlay:                           Visible,
  scienceOffsets:                       Visible,
  acquisitionOffsets:                   Visible,
  elevationPlotRange:                   PlotRange,
  elevationPlotTime:                    TimeDisplay,
  elevationPlotScheduling:              ElevationPlotScheduling,
  itcChartType:                         GraphType,
  itcDetailsOpen:                       PlotDetails,
  elevationPlotElevationVisible:        Visible,
  elevationPlotParallacticAngleVisible: Visible,
  elevationPlotSkyBrightnessVisible:    Visible,
  elevationPlotLunarElevationVisible:   Visible,
  wavelengthUnits:                      WavelengthUnits
) derives Eq,
      Decoder

object GlobalPreferences:
  val aladinMouseScroll                    = Focus[GlobalPreferences](_.aladinMouseScroll)
  val showCatalog                          = Focus[GlobalPreferences](_.showCatalog)
  val agsOverlay                           = Focus[GlobalPreferences](_.agsOverlay)
  val scienceOffsets                       = Focus[GlobalPreferences](_.scienceOffsets)
  val acquisitionOffsets                   = Focus[GlobalPreferences](_.acquisitionOffsets)
  val fullScreen                           = Focus[GlobalPreferences](_.fullScreen)
  val elevationPlotRange                   = Focus[GlobalPreferences](_.elevationPlotRange)
  val elevationPlotTime                    = Focus[GlobalPreferences](_.elevationPlotTime)
  val elevationPlotScheduling              = Focus[GlobalPreferences](_.elevationPlotScheduling)
  val itcChartType                         = Focus[GlobalPreferences](_.itcChartType)
  val itcDetailsOpen                       = Focus[GlobalPreferences](_.itcDetailsOpen)
  val elevationPlotElevationVisible        = Focus[GlobalPreferences](_.elevationPlotElevationVisible)
  val elevationPlotParallacticAngleVisible =
    Focus[GlobalPreferences](_.elevationPlotParallacticAngleVisible)
  val elevationPlotSkyBrightnessVisible    =
    Focus[GlobalPreferences](_.elevationPlotSkyBrightnessVisible)
  val elevationPlotLunarElevationVisible   =
    Focus[GlobalPreferences](_.elevationPlotLunarElevationVisible)
  val wavelengthUnits                      = Focus[GlobalPreferences](_.wavelengthUnits)

  val Default =
    GlobalPreferences(
      AladinMouseScroll.Allowed,
      AladinFullScreen.Normal,
      Visible.Shown,
      Visible.Shown,
      Visible.Shown,
      Visible.Shown,
      PlotRange.Night,
      TimeDisplay.Site,
      ElevationPlotScheduling.On,
      GraphType.S2NGraph,
      PlotDetails.Hidden,
      Visible.Shown,
      Visible.Hidden,
      Visible.Shown,
      Visible.Hidden,
      WavelengthUnits.Nanometers
    )
