// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.TargetVisualOptions
import eu.timepit.refined.scalacheck.numeric.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbAngle.*
import lucuma.core.math.arb.ArbOffset.*
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Cogen.*

trait ArbTargetVisualOptions:

  given Arbitrary[TargetVisualOptions] =
    Arbitrary[TargetVisualOptions] {
      for {
        fra  <- arbitrary[Angle]
        fdec <- arbitrary[Angle]
        vo   <- arbitrary[Offset]
        s    <- arbitrary[TargetVisualOptions.ImageFilterRange]
        b    <- arbitrary[TargetVisualOptions.ImageFilterRange]
      } yield TargetVisualOptions(fra, fdec, vo, s, b)
    }

  given Cogen[TargetVisualOptions] =
    Cogen[
      (Angle, Angle, Offset, Int, Int)
    ].contramap(c => (c.fovRA, c.fovDec, c.viewOffset, c.saturation.value, c.brightness.value))

object ArbTargetVisualOptions extends ArbTargetVisualOptions
