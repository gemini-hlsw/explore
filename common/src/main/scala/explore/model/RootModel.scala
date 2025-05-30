// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.syntax.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.modes.ScienceModes
import explore.syntax.ui.*
import explore.undo.UndoStacks
import lucuma.core.model.GuestUser
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardUser
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.sso.UserVault
import monocle.Focus
import monocle.Lens
import monocle.Optional

import scala.collection.immutable.HashSet

case class RootModel(
  vault:                Option[UserVault],
  localPreferences:     ExploreLocalPreferences,
  expandedIds:          ExpandedIds = ExpandedIds(),
  searchingTarget:      Set[Target.Id] = HashSet.empty,
  userSelectionMessage: Option[NonEmptyString] = none,
  userPreferences:      Pot[UserPreferences] = pending,
  scienceModes:         Pot[ScienceModes] = pending,
  cfps:                 Pot[List[CallForProposal]] = pending,
  undoStacks:           UndoStacks[IO, ProgramSummaries] = UndoStacks.empty[IO, ProgramSummaries],
  otherUndoStacks:      ModelUndoStacks[IO] = ModelUndoStacks[IO]()
) derives Eq:
  lazy val isStaff = vault.isStaff

object RootModel:
  val vault                = Focus[RootModel](_.vault)
  val localPreferences     = Focus[RootModel](_.localPreferences)
  val expandedIds          = Focus[RootModel](_.expandedIds)
  val searchingTarget      = Focus[RootModel](_.searchingTarget)
  val userSelectionMessage = Focus[RootModel](_.userSelectionMessage)
  val userPreferences      = Focus[RootModel](_.userPreferences)
  val scienceModes         = Focus[RootModel](_.scienceModes)
  val cfps                 = Focus[RootModel](_.cfps)
  val undoStacks           = Focus[RootModel](_.undoStacks)
  val otherUndoStacks      = Focus[RootModel](_.otherUndoStacks)

  val userUserId = Lens[User, User.Id](_.id)(s =>
    a =>
      a match {
        case (a: GuestUser)    => a.copy(id = s)
        case (a: ServiceUser)  => a.copy(id = s)
        case (a: StandardUser) => a.copy(id = s)
      }
  )

  val user: Optional[RootModel, User] =
    vault.some.andThen(UserVault.user)

  val userId: Optional[RootModel, User.Id] =
    user.andThen(userUserId)
