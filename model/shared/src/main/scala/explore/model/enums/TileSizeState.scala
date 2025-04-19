// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import cats.Eq

enum TileSizeState:
  case Maximized extends TileSizeState
  case Minimized extends TileSizeState

object TileSizeState:
  given Eq[TileSizeState] = Eq.fromUniversalEquals

  extension (s: TileSizeState)
    inline def isMinimized = s == Minimized
    inline def isMaximized = s == Maximized
