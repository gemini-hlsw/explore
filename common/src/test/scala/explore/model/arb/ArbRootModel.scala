// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.Focused
import explore.model.RootModel
import explore.model.UserVault
import explore.model.arb.all._
import explore.model.enum.AppTab
import lucuma.core.data.EnumZipper
import lucuma.core.data.arb.ArbEnumZipper._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
import org.scalacheck.Gen

trait ArbRootModel {
  import explore.model.arb.ArbFocused._

  implicit val rootModelArb                     = Arbitrary[RootModel] {
    for {
      vault   <- Gen.option(arbitrary[UserVault])
      tabs    <- arbitrary[EnumZipper[AppTab]]
      focused <- arbitrary[Option[Focused]]
    } yield RootModel(vault, tabs, focused)
  }

  implicit def rootModelCogen: Cogen[RootModel] =
    Cogen[(Option[UserVault], EnumZipper[AppTab], Option[Focused])].contramap(m =>
      (m.vault, m.tabs, m.focused)
    )
}

object ArbRootModel extends ArbRootModel
