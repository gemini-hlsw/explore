// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import crystal.Pot
import crystal.react.ThrottlingView
import crystal.react.View
import lucuma.core.enums.ProgramUserRole
import monocle.Focus
import monocle.Lens

case class RootModelViews(
  rootModel:        View[RootModel],
  programSummaries: ThrottlingView[Pot[ProgramSummaries]]
):
  lazy val programSummariesValue: Pot[ProgramSummaries] = programSummaries.throttlerView.get
  lazy val optUserProgramRole: Option[ProgramUserRole]  =
    val users      = programSummariesValue.toOption.flatMap(_.optProgramDetails).map(_.users)
    val thisUserId = rootModel.get.vault.map(_.user.id)
    (thisUserId, users).flatMapN((id, us) => us.find(_.user.id === id)).map(_.role)
  lazy val userIsReadonlyCoi                            = optUserProgramRole.exists(_ === ProgramUserRole.CoiRO)

object RootModelViews:
  val rootModel: Lens[RootModelViews, View[RootModel]]                              =
    Focus[RootModelViews](_.rootModel)
  val programSummaries: Lens[RootModelViews, ThrottlingView[Pot[ProgramSummaries]]] =
    Focus[RootModelViews](_.programSummaries)
