// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.Eq
import cats.syntax.all._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.enum.AppTab
import lucuma.core.data.EnumZipper
import lucuma.core.model.GuestUser
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import monocle.Lens
import monocle.macros.Lenses
import monocle.std.option

@Lenses
case class RootModel(
  vault:                 Option[UserVault],
  tabs:                  EnumZipper[AppTab],
  focused:               Option[Focused] = none,
  targetViewExpandedIds: TargetViewExpandedIds = TargetViewExpandedIds(),
  userSelectionMessage:  Option[NonEmptyString] = none)

object RootModel {
  val userUserId = Lens[User, User.Id](_.id)(s =>
    a =>
      a match {
        case (a: GuestUser)    => a.copy(id = s)
        case (a: ServiceUser)  => a.copy(id = s)
        case (a: StandardUser) => a.copy(id = s)
      }
  )

  val userId =
    RootModel.vault
      .composePrism(option.some)
      .composeLens(UserVault.user)
      .composeLens(userUserId)

  implicit val eqRootModel: Eq[RootModel] =
    Eq.by(m => (m.vault, m.tabs, m.focused, m.targetViewExpandedIds, m.userSelectionMessage))
}
