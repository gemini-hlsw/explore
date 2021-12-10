// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.ExploreLocalPreferences
import org.scalacheck.Arbitrary
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
import org.scalacheck.Gen
import typings.loglevel.mod.LogLevelDesc

trait ArbExploreLocalPreferences {

  implicit val localPreferencesArb = Arbitrary[ExploreLocalPreferences] {
    for {
      level <- Gen.const(LogLevelDesc.INFO)
    } yield ExploreLocalPreferences(level)
  }

  implicit def localPreferencesCogen: Cogen[ExploreLocalPreferences] =
    Cogen[String].contramap(_.level.toString)
}

object ArbExploreLocalPreferences extends ArbExploreLocalPreferences
