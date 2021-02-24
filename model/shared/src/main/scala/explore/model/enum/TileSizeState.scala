// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enum

import lucuma.core.util.Enumerated

sealed abstract class TileSizeState extends Product with Serializable
object TileSizeState {
  case object Maximized extends TileSizeState
  case object Minimized extends TileSizeState
  case object Normal    extends TileSizeState

  /** @group Typeclass Instances */
  implicit val ExecutionEnvironmentEnumerated: Enumerated[TileSizeState] =
    Enumerated.of(Maximized, Minimized, Normal)
}
