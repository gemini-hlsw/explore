// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import explore.model.enum.Display
import lucuma.core.math.Angle
import lucuma.core.math.syntax.int._
import monocle.macros.Lenses

@Lenses
final case class TargetVisualOptions(
  fov:      Display,
  offsets:  Display,
  guiding:  Display,
  probe:    Display,
  posAngle: Angle // This belongs to the observation model
)

object TargetVisualOptions {
  val Default =
    TargetVisualOptions(Display.Hidden, Display.Hidden, Display.Hidden, Display.Hidden, 145.deg)

  implicit val targetVisualOptionsEq: Eq[TargetVisualOptions] =
    Eq.by(x => (x.fov, x.offsets, x.guiding, x.probe, x.posAngle))
}
