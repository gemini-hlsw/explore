// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
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
  aladinMouseScroll: AladinMouseScroll
) derives Eq

object UserGlobalPreferences:
  val aladinMouseScroll = Focus[UserGlobalPreferences](_.aladinMouseScroll)

  val Default =
    UserGlobalPreferences(AladinMouseScroll.Allowed)
