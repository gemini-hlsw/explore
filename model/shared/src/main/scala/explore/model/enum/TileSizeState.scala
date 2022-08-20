// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import cats.Eq

enum TileSizeState:
  case Maximized extends TileSizeState
  case Minimized extends TileSizeState
  case Normal    extends TileSizeState

object TileSizeState:
  given Eq[TileSizeState] = Eq.fromUniversalEquals
