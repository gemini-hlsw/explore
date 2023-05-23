// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
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

case class UserGlobalPreferences(
  aladinMouseScroll:        AladinMouseScroll,
  fullScreen:               AladinFullScreen,
  aladinShowCatalog:        Visible,
  aladinAgsOverlay:         Visible,
  aladinScienceOffsets:     Visible,
  aladinAcquisitionOffsets: Visible
) derives Eq

object UserGlobalPreferences:
  val aladinMouseScroll        = Focus[UserGlobalPreferences](_.aladinMouseScroll)
  val aladinShowCatalog        = Focus[UserGlobalPreferences](_.aladinShowCatalog)
  val aladinAgsOverlay         = Focus[UserGlobalPreferences](_.aladinAgsOverlay)
  val aladinScienceOffsets     = Focus[UserGlobalPreferences](_.aladinScienceOffsets)
  val aladinAcquisitionOffsets = Focus[UserGlobalPreferences](_.aladinAcquisitionOffsets)
  val fullScreen               = Focus[UserGlobalPreferences](_.fullScreen)

  val Default =
    UserGlobalPreferences(
      AladinMouseScroll.Allowed,
      AladinFullScreen.Normal,
      Visible.Shown,
      Visible.Shown,
      Visible.Shown,
      Visible.Shown
    )
