// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import explore.model.enums.PlotRange
import explore.model.enums.TimeDisplay
import explore.model.enums.Visible
import io.circe.Decoder
import lucuma.core.util.NewType
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

case class GlobalPreferences(
  aladinMouseScroll:       AladinMouseScroll,
  fullScreen:              AladinFullScreen,
  showCatalog:             Visible,
  agsOverlay:              Visible,
  scienceOffsets:          Visible,
  acquisitionOffsets:      Visible,
  elevationPlotRange:      PlotRange,
  elevationPlotTime:       TimeDisplay,
  elevationPlotScheduling: ElevationPlotScheduling
) derives Eq,
      Decoder

object GlobalPreferences:
  val aladinMouseScroll       = Focus[GlobalPreferences](_.aladinMouseScroll)
  val showCatalog             = Focus[GlobalPreferences](_.showCatalog)
  val agsOverlay              = Focus[GlobalPreferences](_.agsOverlay)
  val scienceOffsets          = Focus[GlobalPreferences](_.scienceOffsets)
  val acquisitionOffsets      = Focus[GlobalPreferences](_.acquisitionOffsets)
  val fullScreen              = Focus[GlobalPreferences](_.fullScreen)
  val elevationPlotRange      = Focus[GlobalPreferences](_.elevationPlotRange)
  val elevationPlotTime       = Focus[GlobalPreferences](_.elevationPlotTime)
  val elevationPlotScheduling = Focus[GlobalPreferences](_.elevationPlotScheduling)

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
      ElevationPlotScheduling.On
    )
