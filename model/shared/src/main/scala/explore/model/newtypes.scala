// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import lucuma.core.util.NewType

object AladinFullScreen extends NewType[Boolean]:
  inline def FullScreen: AladinFullScreen = AladinFullScreen(true)
  inline def Normal: AladinFullScreen     = AladinFullScreen(false)
  extension (s: AladinFullScreen)
    def flip: AladinFullScreen =
      if (s.value) AladinFullScreen.Normal else AladinFullScreen.FullScreen

type AladinFullScreen = AladinFullScreen.Type

object IsActive extends NewType[Boolean]
type IsActive = IsActive.Type
