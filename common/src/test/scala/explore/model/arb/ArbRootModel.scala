// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.ExploreLocalPreferences
import explore.model.RootModel
import explore.model.UserVault
import explore.model.arb.all._
import explore.model.enum.AppTab
import lucuma.core.data.EnumZipper
import lucuma.core.data.arb.ArbEnumZipper._
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.util.arb.ArbEnumerated._
import lucuma.core.util.arb.ArbGid._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
import org.scalacheck.Gen

trait ArbRootModel {
  import explore.model.arb.ArbExploreLocalPreferences._

  implicit val rootModelArb = Arbitrary[RootModel] {
    for {
      vault         <- Gen.option(arbitrary[UserVault])
      lp            <- arbitrary[ExploreLocalPreferences]
      tabs          <- arbitrary[EnumZipper[AppTab]]
      focusedObs    <- arbitrary[Option[Observation.Id]]
      focusedTarget <- arbitrary[Option[Target.Id]]
    } yield RootModel(vault, tabs, lp, focusedObs, focusedTarget)
  }

  implicit def rootModelCogen: Cogen[RootModel] =
    Cogen[
      (Option[UserVault],
       EnumZipper[AppTab],
       ExploreLocalPreferences,
       Option[Observation.Id],
       Option[Target.Id]
      )
    ]
      .contramap(m => (m.vault, m.tabs, m.localPreferences, m.focusedObs, m.focusedTarget))
}

object ArbRootModel extends ArbRootModel
