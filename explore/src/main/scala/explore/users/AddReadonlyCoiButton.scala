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
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ProgramUserRole
import lucuma.core.model.Program
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.fa.FontAwesomeIcon
import lucuma.react.primereact.Button
import lucuma.react.primereact.TooltipOptions
import lucuma.schemas.ObservationDB.Types.AddProgramUserInput
import lucuma.ui.primereact.*
import queries.common.ProposalQueriesGQL.*

case class AddReadonlyCoiButton(
  programId: Program.Id,
  users:     View[List[ProgramUser]]
) extends ReactFnProps(AddReadonlyCoiButton)

object AddReadonlyCoiButton
    extends ReactFnComponent[AddReadonlyCoiButton](props =>
      for {
        ctx      <- useContext(AppContext.ctx)
        isActive <- useStateView(IsActive(false))
      } yield
        import ctx.given

        def addProgramUser: IO[Unit] =
          val input = AddProgramUserInput(programId = props.programId, role = ProgramUserRole.CoiRO)
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
          tooltip = "Add Co-Investigator",
          onClick = addProgramUser.runAsync
        ).tiny.compact
    )
