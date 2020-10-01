// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.Eq
import explore.model.enum.AppTab
import lucuma.core.data.EnumZipper
import monocle.macros.Lenses

@Lenses
case class RootModel(
  tabs:    EnumZipper[AppTab],
  focused: Option[Focused] = None
)

object RootModel {
  implicit val eqRootModel: Eq[RootModel] = Eq.by(m => (m.tabs, m.focused))
}
