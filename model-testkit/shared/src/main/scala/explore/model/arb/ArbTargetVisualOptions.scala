// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.TargetVisualOptions
import explore.model.enums.Visible
import eu.timepit.refined.scalacheck.numeric.*
import lucuma.core.util.arb.ArbEnumerated.*
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
        a    <- arbitrary[Visible]
        o    <- arbitrary[Visible]
        f    <- arbitrary[Boolean]
        s    <- arbitrary[TargetVisualOptions.ImageFilterRange]
        b    <- arbitrary[TargetVisualOptions.ImageFilterRange]
      } yield TargetVisualOptions(fra, fdec, vo, a, o, f, s, b)
    }

  given Cogen[TargetVisualOptions] =
    Cogen[(Angle, Angle, Offset, Visible, Visible, Boolean)].contramap(c =>
      (c.fovRA, c.fovDec, c.viewOffset, c.agsCandidates, c.agsOverlay, c.fullScreen)
    )

object ArbTargetVisualOptions extends ArbTargetVisualOptions
