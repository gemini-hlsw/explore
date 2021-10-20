// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.Focused
import explore.model.Focused.FocusedObs
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Gen
import lucuma.core.model.Observation
import lucuma.core.util.arb.ArbGid._

trait ArbFocused {
  implicit val focusedArb: Arbitrary[Focused] = Arbitrary(focusedObsGen)

  val focusedObsGen: Gen[Focused.FocusedObs] =
    arbitrary[Observation.Id].map(FocusedObs.apply)

  implicit val focusedObsCogen: Cogen[Focused.FocusedObs] =
    Cogen[Observation.Id].contramap(_.obsId)

  implicit val focusedCogen: Cogen[Focused] =
    Cogen[FocusedObs].contramap { case a: Focused.FocusedObs => a }
}

object ArbFocused extends ArbFocused
