// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.TargetVisualOptions
import explore.model.enum.Visible
import lucuma.core.util.arb.ArbEnumerated._
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbAngle._
import lucuma.core.math.arb.ArbOffset._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._

trait ArbTargetVisualOptions {

  implicit val targetVisualOptionsArb = Arbitrary[TargetVisualOptions] {
    for {
      f  <- arbitrary[Visible]
      fa <- arbitrary[Angle]
      vo <- arbitrary[Offset]
      o  <- arbitrary[Visible]
      g  <- arbitrary[Visible]
      p  <- arbitrary[Visible]
      a  <- arbitrary[Angle]
    } yield TargetVisualOptions(f, fa, vo, o, g, p, a)
  }

  implicit val targetVisualOptionsCogen: Cogen[TargetVisualOptions] =
    Cogen[(Visible, Angle, Offset, Visible, Visible, Visible, Angle)].contramap(c =>
      (c.ccd, c.fovAngle, c.viewOffset, c.offsets, c.guiding, c.probe, c.posAngle)
    )
}

object ArbTargetVisualOptions extends ArbTargetVisualOptions
