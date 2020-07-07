// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.TargetVisualOptions
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._

trait ArbTargetVisualOptions {

  implicit val targetVisualOptionsArb = Arbitrary[TargetVisualOptions] {
    for {
      f <- arbitrary[Boolean]
    } yield TargetVisualOptions(f)
  }

  implicit val targetVisualOptionsCogen: Cogen[TargetVisualOptions] =
    Cogen[Boolean].contramap(c => (c.fov))
}

object ArbTargetVisualOptions extends ArbTargetVisualOptions
