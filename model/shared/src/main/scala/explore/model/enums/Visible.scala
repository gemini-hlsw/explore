// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.util.NewBoolean

/** Visibility of an item. */
object Visible extends NewBoolean:
  inline def Shown = True; inline def Hidden = False
  extension (self: Visible)
    def fold[A](hidden: => A, shown: => A): A = if self then shown else hidden
    def flip: Visible                         = fold(Visible.Shown, Visible.Hidden)
    def isVisible: Boolean                    = self
type Visible = Visible.Type
