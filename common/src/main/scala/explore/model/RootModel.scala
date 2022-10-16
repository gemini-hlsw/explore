// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import cats.effect.IO
import cats.syntax.all.*
import eu.timepit.refined.cats.*
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.Band
import lucuma.core.model.GuestUser
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardUser
import lucuma.core.model.Target
import lucuma.core.model.User
import monocle.Focus
import monocle.Lens
import monocle.Optional

import scala.collection.immutable.HashSet

case class RootModel(
  vault:                      Option[UserVault],
  localPreferences:           ExploreLocalPreferences,
  expandedIds:                ExpandedIds = ExpandedIds(),
  searchingTarget:            Set[Target.Id] = HashSet.empty,
  userSelectionMessage:       Option[NonEmptyString] = none,
  targetSummaryHiddenColumns: Set[String] =
    Set("epoch", "pmra", "pmdec", "z", "cz", "parallax", "morphology", "sed") ++
      Band.all
        .filterNot(_ === Band.V)
        .map(b => b.shortName + "mag"),
  undoStacks:                 ModelUndoStacks[IO] = ModelUndoStacks[IO]()
) derives Eq

object RootModel {
  val vault                      = Focus[RootModel](_.vault)
  val userSelectionMessage       = Focus[RootModel](_.userSelectionMessage)
  val searchingTarget            = Focus[RootModel](_.searchingTarget)
  val undoStacks                 = Focus[RootModel](_.undoStacks)
  val expandedIds                = Focus[RootModel](_.expandedIds)
  val targetSummaryHiddenColumns = Focus[RootModel](_.targetSummaryHiddenColumns)
  val localPreferences           = Focus[RootModel](_.localPreferences)

  val userUserId = Lens[User, User.Id](_.id)(s =>
    a =>
      a match {
        case (a: GuestUser)    => a.copy(id = s)
        case (a: ServiceUser)  => a.copy(id = s)
        case (a: StandardUser) => a.copy(id = s)
      }
  )

  val user: Optional[RootModel, User] =
    RootModel.vault.some.andThen(UserVault.user)

  val userId: Optional[RootModel, User.Id] =
    user.andThen(userUserId)

}
