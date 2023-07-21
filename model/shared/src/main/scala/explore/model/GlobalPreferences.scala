// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import explore.model.enums.PlotRange
import explore.model.enums.TimeDisplay
import explore.model.enums.Visible
import lucuma.core.util.NewType
import monocle.Focus

object AladinMouseScroll extends NewType[Boolean]:
  inline def Allowed: AladinMouseScroll  = AladinMouseScroll(true)
  inline def Disabled: AladinMouseScroll = AladinMouseScroll(false)
  extension (s: AladinMouseScroll)
    inline def flip: AladinMouseScroll =
      if (s.value) AladinMouseScroll.Disabled else AladinMouseScroll.Allowed

type AladinMouseScroll = AladinMouseScroll.Type

case class GlobalPreferences(
  aladinMouseScroll:        AladinMouseScroll,
  fullScreen:               AladinFullScreen,
  aladinShowCatalog:        Visible,
  aladinAgsOverlay:         Visible,
  aladinScienceOffsets:     Visible,
  aladinAcquisitionOffsets: Visible,
  plotRange:                PlotRange,
  timeDisplay:              TimeDisplay
) derives Eq

object GlobalPreferences:
  val aladinMouseScroll        = Focus[GlobalPreferences](_.aladinMouseScroll)
  val aladinShowCatalog        = Focus[GlobalPreferences](_.aladinShowCatalog)
  val aladinAgsOverlay         = Focus[GlobalPreferences](_.aladinAgsOverlay)
  val aladinScienceOffsets     = Focus[GlobalPreferences](_.aladinScienceOffsets)
  val aladinAcquisitionOffsets = Focus[GlobalPreferences](_.aladinAcquisitionOffsets)
  val fullScreen               = Focus[GlobalPreferences](_.fullScreen)
  val plotRange                = Focus[GlobalPreferences](_.plotRange)
  val timeDisplay              = Focus[GlobalPreferences](_.timeDisplay)

  val Default =
    GlobalPreferences(
      AladinMouseScroll.Allowed,
      AladinFullScreen.Normal,
      Visible.Shown,
      Visible.Shown,
      Visible.Shown,
      Visible.Shown,
      PlotRange.Night,
      TimeDisplay.Site
    )
