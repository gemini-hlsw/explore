// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.arb

import explore.model.Focused
import explore.model.RootModel
import explore.model.enum.AppTab
import gem.util.Enumerated
import gem.data.EnumZipper
import gem.arb.ArbEnumZipper._
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary._
import org.scalacheck.Cogen
import org.scalacheck.Cogen._
import org.scalacheck.Gen
import org.scalacheck.Gen._

trait ArbRootModel {
  import explore.model.arb.ArbFocused._

  implicit val rootModelArb = Arbitrary[RootModel] {
    for {
      tabs    <- arbitrary[EnumZipper[AppTab]]
      focused <- arbitrary[Option[Focused]]
    } yield RootModel(tabs, focused)
  }

  implicit def rootModelCogen: Cogen[RootModel] =
    Cogen[(EnumZipper[AppTab], Option[Focused])].contramap(m => (m.tabs, m.focused))
}

object ArbRootModel extends ArbRootModel
