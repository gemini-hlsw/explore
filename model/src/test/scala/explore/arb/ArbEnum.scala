// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.enum.AppTab
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
import gem.util.Enumerated
import org.scalacheck.Gen

trait ArbEnum {
  implicit def enumArb[A: Enumerated]: Arbitrary[A] =
    Arbitrary[A] {
      Gen.oneOf(implicitly[Enumerated[A]].all)
    }

  implicit def enumCogen[A: Enumerated]: Cogen[A] =
    Cogen[String].contramap(a => implicitly[Enumerated[A]].tag(a))
}

object ArbEnum extends ArbEnum
