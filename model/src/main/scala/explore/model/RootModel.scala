// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

// import java.time.Instant

import scala.collection.immutable.SortedSet

import cats.Order._
import cats.kernel.Eq
import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.enum.AppTab
import lucuma.core.data.EnumZipper
import lucuma.core.model.Target
import lucuma.core.model.Target.Id._
import monocle.macros.Lenses

@Lenses
case class RootModel(
  vault:                Option[UserVault],
  tabs:                 EnumZipper[AppTab],
  focused:              Option[Focused] = none,
  expandedTargetIds:    SortedSet[Target.Id] = SortedSet.empty,
  userSelectionMessage: Option[NonEmptyString] = none
)

object RootModel {
  implicit val eqRootModel: Eq[RootModel] = Eq.by(m => (m.vault, m.tabs, m.focused))
}
