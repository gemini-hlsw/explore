// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.users

import cats.effect.IO
import crystal.react.*
import crystal.react.hooks.*
import explore.Icons
import explore.model.AppContext
import explore.model.IsActive
import explore.model.ProgramUser
import explore.model.display.given
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.Program
import lucuma.core.syntax.display.*
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.fa.FontAwesomeIcon
import lucuma.react.primereact.Button
import lucuma.ui.primereact.*

case class AddProgramUserButton(
  programId: Program.Id,
  role:      ProgramUserRole,
  users:     View[List[ProgramUser]]
) extends ReactFnProps(AddProgramUserButton)

object AddProgramUserButton
    extends ReactFnComponent[AddProgramUserButton](props =>
      for {
        ctx      <- useContext(AppContext.ctx)
        isActive <- useStateView(IsActive(false))
      } yield
        import ctx.given

        def addProgramUser: IO[Unit] =
          ctx.odbApi
            .addProgramUser(props.programId, props.role)
            .flatMap(pu => props.users.mod(_ :+ pu).to[IO])
            .switching(isActive.async, IsActive(_))

        val toolTip = props.role match
          case ProgramUserRole.External => "Share data with a new user"
          case _                        => s"Add ${props.role.longName}"

        Button(
          severity = Button.Severity.Secondary,
          loading = isActive.get.value,
          icon = Icons.UserPlus,
          tooltip = toolTip,
          onClick = addProgramUser.runAsync
        ).tiny.compact
    )
