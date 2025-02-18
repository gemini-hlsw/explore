// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.Program
import lucuma.core.syntax.display.*
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.fa.FontAwesomeIcon
import lucuma.react.primereact.Button
import lucuma.react.primereact.TooltipOptions
import lucuma.schemas.ObservationDB.Types.AddProgramUserInput
import lucuma.ui.primereact.*
import queries.common.ProposalQueriesGQL.*

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
          val input = AddProgramUserInput(programId = props.programId, role = props.role)
          AddProgramUser[IO]
            .execute(input)
            .raiseGraphQLErrors
            .map(_.addProgramUser.programUser)
            .flatMap(pu => props.users.mod(_ :+ pu).to[IO])
            .switching(isActive.async, IsActive(_))

        Button(
          severity = Button.Severity.Secondary,
          loading = isActive.get.value,
          icon = Icons.UserPlus,
          tooltip = s"Add ${props.role.longName}",
          onClick = addProgramUser.runAsync
        ).tiny.compact
    )
