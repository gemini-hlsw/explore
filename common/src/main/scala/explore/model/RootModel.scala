// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.effect.IO
import cats.syntax.all._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.string.NonEmptyString
import explore.model.enum.AppTab
import lucuma.core.data.EnumZipper
import lucuma.core.enum.MagnitudeBand
import lucuma.core.model.GuestUser
import lucuma.core.model.ServiceUser
import lucuma.core.model.StandardUser
import lucuma.core.model.User
import monocle.Focus
import monocle.Lens

import scala.collection.immutable.HashSet

case class RootModel(
  vault:                          Option[UserVault],
  tabs:                           EnumZipper[AppTab],
  focused:                        Option[Focused] = none,
  expandedIds:                    ExpandedIds = ExpandedIds(),
  searchingTarget:                Set[ScienceTarget.Id] = HashSet.empty,
  userSelectionMessage:           Option[NonEmptyString] = none,
  targetSummaryHiddenColumns:     Set[String] =
    Set("epoch", "pmra", "pmdec", "z", "cz", "parallax", "morphology", "sed") ++
      MagnitudeBand.all
        .filterNot(_ === MagnitudeBand.V)
        .map(b => (b.shortName + "mag")),
  constraintSummaryHiddenColumns: Set[String] = Set("minam", "minha", "maxha"),
  constraintSummarySorting:       List[(String, Boolean)] = List.empty,
  undoStacks:                     ModelUndoStacks[IO] = ModelUndoStacks[IO]()
)

object RootModel {
  val vault                          = Focus[RootModel](_.vault)
  val focused                        = Focus[RootModel](_.focused)
  val userSelectionMessage           = Focus[RootModel](_.userSelectionMessage)
  val tabs                           = Focus[RootModel](_.tabs)
  val searchingTarget                = Focus[RootModel](_.searchingTarget)
  val undoStacks                     = Focus[RootModel](_.undoStacks)
  val expandedIds                    = Focus[RootModel](_.expandedIds)
  val targetSummaryHiddenColumns     = Focus[RootModel](_.targetSummaryHiddenColumns)
  val constraintSummaryHiddenColumns = Focus[RootModel](_.constraintSummaryHiddenColumns)
  val constraintSummarySorting       = Focus[RootModel](_.constraintSummarySorting)

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
       m.tabs,
       m.focused,
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
