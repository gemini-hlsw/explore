// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import explore.model.enum.Visible
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import monocle.Focus

final case class TargetVisualOptions(
  fovAngle:      Angle,
  viewOffset:    Offset,
  agsCandidates: Visible
)

object TargetVisualOptions {
  val fovAngle      = Focus[TargetVisualOptions](_.fovAngle)
  val viewOffset    = Focus[TargetVisualOptions](_.viewOffset)
  val agsCandidates = Focus[TargetVisualOptions](_.agsCandidates)

  val Default =
    TargetVisualOptions(Constants.InitialFov, Offset.Zero, Visible.Hidden)

  implicit val targetVisualOptionsEq: Eq[TargetVisualOptions] =
    Eq.by(x => (x.fovAngle, x.viewOffset, x.agsCandidates))
}
