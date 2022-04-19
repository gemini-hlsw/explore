// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import explore.model.enum.Visible
import lucuma.core.math.Angle
import lucuma.core.math.syntax.int._
import monocle.Focus

final case class TargetVisualOptions(
  ccd:      Visible,
  fov:      Option[Angle],
  offsets:  Visible,
  guiding:  Visible,
  probe:    Visible,
  posAngle: Angle // This belongs to the observation model
)

object TargetVisualOptions {
  val ccd      = Focus[TargetVisualOptions](_.ccd)
  val fov      = Focus[TargetVisualOptions](_.fov)
  val offsets  = Focus[TargetVisualOptions](_.offsets)
  val guiding  = Focus[TargetVisualOptions](_.guiding)
  val probe    = Focus[TargetVisualOptions](_.probe)
  val posAngle = Focus[TargetVisualOptions](_.posAngle)

  val Default =
    TargetVisualOptions(Visible.Hidden,
                        None,
                        Visible.Hidden,
                        Visible.Hidden,
                        Visible.Hidden,
                        145.deg
    )

  implicit val targetVisualOptionsEq: Eq[TargetVisualOptions] =
    Eq.by(x => (x.ccd, x.fov, x.offsets, x.guiding, x.probe, x.posAngle))
}
