// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import explore.model.enum.Visible
import lucuma.core.math.Angle
import lucuma.core.math.syntax.int._
import monocle.macros.Lenses

@Lenses
final case class TargetVisualOptions(
  fov:      Visible,
  fovAngle: Angle,
  offsets:  Visible,
  guiding:  Visible,
  probe:    Visible,
  posAngle: Angle // This belongs to the observation model
)

object TargetVisualOptions {
  val Default =
    TargetVisualOptions(Visible.Hidden,
                        Constants.InitialFov,
                        Visible.Hidden,
                        Visible.Hidden,
                        Visible.Hidden,
                        145.deg
    )

  implicit val targetVisualOptionsEq: Eq[TargetVisualOptions] =
    Eq.by(x => (x.fov, x.fovAngle, x.offsets, x.guiding, x.probe, x.posAngle))
}
