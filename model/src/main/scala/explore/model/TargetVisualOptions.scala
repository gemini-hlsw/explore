// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats._
import cats.implicits._
import monocle.macros.Lenses
import explore.model.enum.Display

@Lenses
final case class TargetVisualOptions(
  fov:     Display,
  offsets: Display,
  guiding: Display,
  probe:   Display
)

object TargetVisualOptions {
  val Default = TargetVisualOptions(Display.Hidden, Display.Hidden, Display.Hidden, Display.Hidden)

  implicit val targetVisualOptionsEq: Eq[TargetVisualOptions] =
    Eq.by(x => (x.fov, x.offsets, x.guiding, x.probe))
}

