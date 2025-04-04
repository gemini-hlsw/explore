// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import lucuma.core.util.NewBoolean

object AladinFullScreen extends NewBoolean:
  inline def FullScreen = True; inline def Normal = False
type AladinFullScreen = AladinFullScreen.Type

object IsActive extends NewBoolean
type IsActive = IsActive.Type

object PopupState extends NewBoolean { inline def Open = True; inline def Closed = False }
type PopupState = PopupState.Type
