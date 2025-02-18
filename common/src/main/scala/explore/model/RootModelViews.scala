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

  // Can be both some sort of Support AND COI. So, can have multiple roles
  lazy val userProgramRoles: List[ProgramUserRole] =
    val users      = programSummariesValue.toOption.flatMap(_.optProgramDetails).map(_.users)
    val thisUserId = rootModel.get.vault.map(_.user.id)
    (thisUserId, users)
      .mapN((id, us) => us.filter(_.user.exists(_.id === id)).map(_.role))
      .orEmpty

  def hasRole(role: ProgramUserRole): Boolean =
    userProgramRoles.exists(_ === role)

  lazy val userIsReadonlyCoi: Boolean =
    val isStaff = rootModel.get.isStaff
    !isStaff && hasRole(ProgramUserRole.CoiRO) && !(hasRole(
      ProgramUserRole.SupportPrimary
    ) || hasRole(ProgramUserRole.SupportSecondary))

  lazy val userIsPi: Boolean =
    val thisUserId = rootModel.get.vault.map(_.user.id)
    val pi         =
      programSummariesValue.toOption.flatMap(_.optProgramDetails).flatMap(_.pi).flatMap(_.user)
    (thisUserId, pi).mapN(_ === _.id).getOrElse(false)

object RootModelViews:
  val rootModel: Lens[RootModelViews, View[RootModel]]                              =
    Focus[RootModelViews](_.rootModel)
  val programSummaries: Lens[RootModelViews, ThrottlingView[Pot[ProgramSummaries]]] =
    Focus[RootModelViews](_.programSummaries)
