// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.ExploreLocalPreferences
import explore.model.RootModel
import lucuma.ui.sso.UserVault
import lucuma.ui.sso.arb.ArbUserVault.given
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.*
import org.scalacheck.Cogen
import org.scalacheck.Cogen.*
import org.scalacheck.Gen

trait ArbRootModel {
  import explore.model.arb.ArbExploreLocalPreferences.*

  implicit val rootModelArb: Arbitrary[RootModel] = Arbitrary[RootModel] {
    for {
      vault <- Gen.option(arbitrary[UserVault])
      lp    <- arbitrary[ExploreLocalPreferences]
    } yield RootModel(vault, lp)
  }

  implicit def rootModelCogen: Cogen[RootModel] =
    Cogen[
      (Option[UserVault], ExploreLocalPreferences)
    ]
      .contramap(m => (m.vault, m.localPreferences))
}

object ArbRootModel extends ArbRootModel
