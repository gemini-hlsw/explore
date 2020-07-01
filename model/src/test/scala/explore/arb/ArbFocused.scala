// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import cats.implicits._
import explore.model.ExploreObservation
import explore.model.Focused
import explore.model.Focused.FocusedObs
import explore.model.Focused.FocusedTarget
import explore.model.SiderealTarget
import gem.Observation
import gem.arb.ArbObservation._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
import org.scalacheck.Gen
import org.scalacheck.Gen._

trait ArbFocused {
  import explore.model.arb.ArbSiderealTarget._
  import explore.model.arb.CogenUUID._

  implicit val focusedArb: Arbitrary[Focused] =
    Arbitrary(
      oneOf(genFocusedObs, genFocusedTarget)
    )

  val genFocusedObs: Gen[Focused.FocusedObs] =
    arbitrary[ExploreObservation.Id].map(FocusedObs.apply)

  val genFocusedTarget: Gen[Focused.FocusedTarget] =
    arbitrary[SiderealTarget.Id].map(FocusedTarget.apply)

  implicit val focusedObsCogen: Cogen[Focused.FocusedObs] =
    Cogen[ExploreObservation.Id].contramap(_.obsId)

  implicit val focusedTargetCogen: Cogen[Focused.FocusedTarget] =
    Cogen[SiderealTarget.Id].contramap(_.targetId)

  implicit val focusedCoge: Cogen[Focused] =
    Cogen[Either[Focused.FocusedObs, Focused.FocusedTarget]].contramap {
      case a: Focused.FocusedObs    => a.asLeft
      case a: Focused.FocusedTarget => a.asRight
    }
}

object ArbFocused extends ArbFocused
