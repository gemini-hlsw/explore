// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import explore.model.enums.PlotRange
import explore.model.enums.TimeDisplay
import explore.model.enums.Visible
import explore.model.itc.PlotDetails
import io.circe.Decoder
import lucuma.core.util.NewType
import lucuma.itc.GraphType
import monocle.Focus

object AladinMouseScroll extends NewType[Boolean]:
  inline def Allowed: AladinMouseScroll  = AladinMouseScroll(true)
  inline def Disabled: AladinMouseScroll = AladinMouseScroll(false)
  extension (s: AladinMouseScroll)
    inline def flip: AladinMouseScroll =
      if (s.value) AladinMouseScroll.Disabled else AladinMouseScroll.Allowed

type AladinMouseScroll = AladinMouseScroll.Type

object ElevationPlotScheduling extends NewType[Boolean]:
  val On: ElevationPlotScheduling  = ElevationPlotScheduling(true)
  val Off: ElevationPlotScheduling = ElevationPlotScheduling(false)
  extension (s: ElevationPlotScheduling)
    inline def flip: ElevationPlotScheduling =
      if (s.value) ElevationPlotScheduling.Off else ElevationPlotScheduling.On

type ElevationPlotScheduling = ElevationPlotScheduling.Type

given Decoder[GraphType] = Decoder.decodeString.map:
  case "S2N_CHART" => GraphType.S2NGraph

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
  elevationPlotLunarElevationVisible:   Visible
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
      Visible.Hidden
    )
