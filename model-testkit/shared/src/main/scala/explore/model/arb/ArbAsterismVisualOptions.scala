// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.AsterismVisualOptions
import eu.timepit.refined.scalacheck.numeric.*
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.math.arb.ArbAngle.given
import lucuma.core.math.arb.ArbOffset.given
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Cogen.*

trait ArbAsterismVisualOptions:

  given Arbitrary[AsterismVisualOptions] =
    Arbitrary[AsterismVisualOptions] {
      for {
        id   <- arbitrary[Option[Int]]
        fra  <- arbitrary[Angle]
        fdec <- arbitrary[Angle]
        vo   <- arbitrary[Offset]
        s    <- arbitrary[AsterismVisualOptions.ImageFilterRange]
        b    <- arbitrary[AsterismVisualOptions.ImageFilterRange]
      } yield AsterismVisualOptions(id, fra, fdec, vo, s, b)
    }

  given Cogen[AsterismVisualOptions] =
    Cogen[
      (Option[Int], Angle, Angle, Offset, Int, Int)
    ].contramap(c =>
      (c.id, c.fovRA, c.fovDec, c.viewOffset, c.saturation.value, c.brightness.value)
    )

object ArbAsterismVisualOptions extends ArbAsterismVisualOptions
