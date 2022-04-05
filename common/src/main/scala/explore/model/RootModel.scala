// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.effect.IO
import cats.syntax.all._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enum.Band
import lucuma.core.model.GuestUser
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardUser
import lucuma.core.model.Target
import lucuma.core.model.User
import monocle.Focus
import monocle.Lens

import scala.collection.immutable.HashSet

case class RootModel(
  vault:                          Option[UserVault],
  localPreferences:               ExploreLocalPreferences,
  expandedIds:                    ExpandedIds = ExpandedIds(),
  searchingTarget:                Set[Target.Id] = HashSet.empty,
  userSelectionMessage:           Option[NonEmptyString] = none,
  targetSummaryHiddenColumns:     Set[String] =
    Set("epoch", "pmra", "pmdec", "z", "cz", "parallax", "morphology", "sed") ++
      Band.all
        .filterNot(_ === Band.V)
        .map(b => (b.shortName + "mag")),
  constraintSummaryHiddenColumns: Set[String] = Set("minam", "minha", "maxha"),
  constraintSummarySorting:       List[(String, Boolean)] = List.empty,
  undoStacks:                     ModelUndoStacks[IO] = ModelUndoStacks[IO]()
)

object RootModel {
  val vault                          = Focus[RootModel](_.vault)
  val userSelectionMessage           = Focus[RootModel](_.userSelectionMessage)
  val searchingTarget                = Focus[RootModel](_.searchingTarget)
  val undoStacks                     = Focus[RootModel](_.undoStacks)
  val expandedIds                    = Focus[RootModel](_.expandedIds)
  val targetSummaryHiddenColumns     = Focus[RootModel](_.targetSummaryHiddenColumns)
  val constraintSummaryHiddenColumns = Focus[RootModel](_.constraintSummaryHiddenColumns)
  val constraintSummarySorting       = Focus[RootModel](_.constraintSummarySorting)
  val localPreferences               = Focus[RootModel](_.localPreferences)

  val userUserId = Lens[User, User.Id](_.id)(s =>
    a =>
      a match {
        case (a: GuestUser)    => a.copy(id = s)
        case (a: ServiceUser)  => a.copy(id = s)
        case (a: StandardUser) => a.copy(id = s)
      }
  )

  val userId =
    RootModel.vault.some
      .andThen(UserVault.user)
      .andThen(userUserId)

  implicit val eqRootModel: Eq[RootModel] =
    Eq.by(m =>
      (m.vault,
       m.localPreferences,
       m.expandedIds,
       m.searchingTarget,
       m.userSelectionMessage,
       m.targetSummaryHiddenColumns,
       m.constraintSummaryHiddenColumns,
       m.constraintSummarySorting,
       m.undoStacks
      )
    )
}
