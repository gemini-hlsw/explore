// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.SideButton
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._

trait ArbSideButton {

  implicit val sideButtonArb = Arbitrary[SideButton] {
    for {
      n <- arbitrary[String]
    } yield SideButton(n)
  }

  implicit val sideButtonCogen: Cogen[SideButton] =
    Cogen[String].contramap(c => (c.title))

}

object ArbSideButton extends ArbSideButton
