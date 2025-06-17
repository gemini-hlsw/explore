// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.StandardRole
import lucuma.core.model.StandardUser
import lucuma.react.common.*
import lucuma.react.primereact.SelectItem
import lucuma.refined.*
import lucuma.ui.primereact.FormDropdown
import lucuma.ui.sso.SSOClient
import lucuma.ui.sso.UserVault
import lucuma.ui.syntax.all.given

case class RoleSwitch(
  vault:        View[UserVault],
  ssoClient:    SSOClient[IO],
  onRoleChange: StandardRole => IO[Unit]
) extends ReactFnProps(RoleSwitch)

object RoleSwitch
    extends ReactFnComponent[RoleSwitch](props =>
      val user = props.vault.get.user

      def roleSwitch(role: StandardRole) =
        (
          for
            t <- props.ssoClient.switchRole(role.id)
            _ <- t.foldMap(props.vault.set(_).toAsync)
            _ <- props.onRoleChange(role)
          yield ()
        ).runAsyncAndForget

      val (curRole, otherRoles) = user match {
        case StandardUser(_, role, other, _) => (role.some, other)
        case _                               => (none, Nil)
      }

      React.Fragment(
        curRole match {
          case Some(r) if otherRoles.nonEmpty =>
            val options: List[SelectItem[StandardRole]] =
              (r :: otherRoles).map(r => SelectItem(r, label = r.name))

            FormDropdown(
              id = "role-selector-switch".refined,
              r,
              options,
              onChange = roleSwitch
            )
          case a                              =>
            EmptyVdom
        }
      )
    )
