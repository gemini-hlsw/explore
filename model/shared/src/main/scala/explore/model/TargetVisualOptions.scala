// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import explore.model.enums.Visible
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import monocle.Focus

case class TargetVisualOptions(
  fovRA:         Angle,
  fovDec:        Angle,
  viewOffset:    Offset,
  agsCandidates: Visible,
  agsOverlay:    Visible,
  fullScreen:    Boolean
) derives Eq

object TargetVisualOptions:
  val fovRA         = Focus[TargetVisualOptions](_.fovRA)
  val fovDec        = Focus[TargetVisualOptions](_.fovDec)
  val viewOffset    = Focus[TargetVisualOptions](_.viewOffset)
  val agsCandidates = Focus[TargetVisualOptions](_.agsCandidates)
  val agsOverlay    = Focus[TargetVisualOptions](_.agsOverlay)
  val fullScreen    = Focus[TargetVisualOptions](_.fullScreen)

  val Default =
    TargetVisualOptions(Constants.InitialFov,
                        Constants.InitialFov,
                        Offset.Zero,
                        Visible.Hidden,
                        Visible.Hidden,
                        false
    )
