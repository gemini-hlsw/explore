// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.FocusedObs
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import lucuma.core.model.Observation
import lucuma.core.util.arb.ArbGid._

trait ArbFocusedObs {
  implicit val focusedObsGen: Arbitrary[FocusedObs] = Arbitrary {
    arbitrary[Observation.Id].map(FocusedObs.apply)
  }

  implicit val focusedObsCogen: Cogen[FocusedObs] =
    Cogen[Observation.Id].contramap(_.obsId)
}

object ArbFocusedObs extends ArbFocusedObs
