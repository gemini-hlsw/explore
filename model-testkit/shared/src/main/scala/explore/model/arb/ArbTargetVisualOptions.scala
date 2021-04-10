// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.TargetVisualOptions
import explore.model.enum.Display
import lucuma.core.util.arb.ArbEnumerated._
import lucuma.core.math.Angle
import lucuma.core.math.arb.ArbAngle._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._

trait ArbTargetVisualOptions {

  implicit val targetVisualOptionsArb = Arbitrary[TargetVisualOptions] {
    for {
      f  <- arbitrary[Display]
      fa <- arbitrary[Angle]
      o  <- arbitrary[Display]
      g  <- arbitrary[Display]
      p  <- arbitrary[Display]
      a  <- arbitrary[Angle]
    } yield TargetVisualOptions(f, fa, o, g, p, a)
  }

  implicit val targetVisualOptionsCogen: Cogen[TargetVisualOptions] =
    Cogen[(Display, Display, Display, Display, Angle)].contramap(c =>
      (c.fov, c.offsets, c.guiding, c.probe, c.posAngle)
    )
}

object ArbTargetVisualOptions extends ArbTargetVisualOptions
