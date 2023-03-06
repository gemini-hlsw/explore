// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import clue.TransactionalClient
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import crystal.react.reuse.*
import explore.EditableLabel
import explore.Icons
import explore.*
import explore.common.ProgramQueries
import explore.common.ProgramQueries.ProgramInfo
import explore.common.ProgramQueries.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Focused
import explore.model.enums.AppTab
import explore.syntax.ui.given
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.util.NewType
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.schemas.ObservationDB
import lucuma.typed.{tanstackTableCore => raw}
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.utils.*
import org.typelevel.log4cats.Logger
import queries.common.ProgramQueriesGQL.*
import react.common.ReactFnProps
import react.floatingui.Placement
import react.floatingui.syntax.*
import react.primereact.Button

case class ProgramTable(
  currentProgramId: Option[Program.Id],
  selectProgram:    Program.Id => Callback,
  isRequired:       Boolean,
  onClose:          Option[Callback]
) extends ReactFnProps(ProgramTable.component)

object ProgramTable:
  private type Props = ProgramTable

  private val ColDef = ColumnDef[View[ProgramInfo]]

  private object IsAdding extends NewType[Boolean]
  private type IsAdding = IsAdding.Type

  private object ShowDeleted extends NewType[Boolean]
  private type ShowDeleted = ShowDeleted.Type

  private def addProgram(programs: View[List[ProgramInfo]], adding: View[IsAdding])(using
    TransactionalClient[IO, ObservationDB],
    Logger[IO]
  ): IO[Unit] =
    adding.async.set(IsAdding(true)) >>
      ProgramQueries
        .createProgram[IO](none)
        .flatMap(pi => programs.async.mod(_ :+ pi))
        .guarantee(adding.async.set(IsAdding(false)))

  private def deleteProgram(pinf: View[ProgramInfo])(using
    TransactionalClient[IO, ObservationDB]
  ): IO[Unit] =
    pinf.zoom(ProgramInfo.deleted).set(true).to[IO] >>
      ProgramQueries.deleteProgram[IO](pinf.get.id)

  private def undeleteProgram(pinf: View[ProgramInfo])(using
    TransactionalClient[IO, ObservationDB],
    Logger[IO]
  ): IO[Unit] =
    pinf.zoom(ProgramInfo.deleted).set(false).to[IO] >>
      ProgramQueries.undeleteProgram[IO](pinf.get.id)

  private def onModName(pinf: ProgramInfo)(using
    TransactionalClient[IO, ObservationDB],
    Logger[IO]
  ): Callback =
    ProgramQueries.updateProgramName[IO](pinf.id, pinf.name).runAsync

  private def onNewData(
    isRequired: Boolean,
    programs:   List[ProgramInfo],
    ctx:        AppContext[IO]
  ): IO[Unit] =
    (isRequired, programs) match
      case (true, head :: Nil) => ctx.replacePage(AppTab.Overview, head.id, Focused.None).to[IO]
      case _                   => IO.unit

  private val ActionsColumnId: ColumnId = ColumnId("actions")
  private val IdColumnId: ColumnId      = ColumnId("id")
  private val NameColumnId: ColumnId    = ColumnId("name")

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useStateView(IsAdding(false))    // Adding new program
    .useStateView(ShowDeleted(false)) // Show deleted
    .useStreamResourceViewBy((_, _, _, showDeleted) => showDeleted.get) {
      (props, ctx, _, _) => showDeleted =>
        import ctx.given

        ProgramsQuery
          .query(includeDeleted = showDeleted.value)
          .map(ProgramsQuery.Data.asProgramInfoList)
          .flatTap(programs => onNewData(props.isRequired, programs, ctx))
          .reRunOnResourceSignals(ProgramEditSubscription.subscribe[IO]())
    }
    // Columns
    .useMemoBy((props, _, _, _, programs) =>
      (props.currentProgramId, programs.toOption.map(_.get.length).orEmpty)
    ) { (props, ctx, _, _, _) => (currentProgramId, programCount) =>
      import ctx.given

      List(
        ColDef(
          ActionsColumnId,
          identity[View[ProgramInfo]] _,
          "Actions",
          cell = { cell =>
            val programId = cell.value.get.id
            val isDeleted = cell.value.get.deleted
            <.div(
              Button(
                label = "Select",
                icon = Icons.Checkmark,
                severity = Button.Severity.Secondary,
                disabled = currentProgramId.exists(_ === programId),
                onClick = props.selectProgram(programId)
              ).compact.mini.unless(cell.row.original.get.deleted),
              Button(
                icon = Icons.Trash,
                severity = Button.Severity.Secondary,
                disabled = currentProgramId.exists(_ === programId) || programCount < 2,
                onClick = deleteProgram(cell.value).runAsync,
                tooltip = "Delete program"
              ).mini.compact.unless(isDeleted),
              Button(
                label = "Undelete",
                icon = Icons.TrashUndo,
                severity = Button.Severity.Secondary,
                onClick = undeleteProgram(cell.value).runAsync,
                tooltip = "Undelete program"
              ).mini.compact.when(isDeleted)
            )
          },
          size = 100.toPx,
          minSize = 100.toPx,
          maxSize = 120.toPx
        ).sortableBy(_.get.deleted),
        ColDef(
          IdColumnId,
          _.get.id,
          "Id",
          _.value.toString,
          size = 50.toPx,
          minSize = 50.toPx,
          maxSize = 70.toPx
        ).sortable,
        ColDef(
          NameColumnId,
          _.withOnMod(onModName).zoom(ProgramInfo.name),
          "Name",
          cell =>
            EditableLabel
              .fromView(
                value = cell.value,
                addButtonLabel = ("Add program name": VdomNode).reuseAlways,
                textClass = ExploreStyles.ProgramName,
                inputClass = ExploreStyles.ProgramNameInput,
                editButtonTooltip = "Edit program name".some,
                deleteButtonTooltip = "Delete program name".some,
                okButtonTooltip = "Accept".some,
                discardButtonTooltip = "Discard".some
              ),
          size = 500.toPx,
          minSize = 200.toPx,
          maxSize = 1000.toPx
        ).sortableBy(_.get.foldMap(_.value))
      )
    }
    // Rows
    .useMemoBy((_, _, _, showDeleted, programs, _) =>
      (programs.toOption.map(_.reuseByValue), showDeleted.get)
    ) { (_, _, _, _, _, _) => (programs, showDeleted) =>
      programs
        .map(
          _.value.toListOfViews
            .filter(vpi => showDeleted.value || !vpi.get.deleted)
            .sortBy(_.get.id)
        )
        .orEmpty
    }
    .useReactTableBy((_, _, _, _, _, cols, rows) =>
      TableOptions(cols, rows, enableSorting = true, enableColumnResizing = false)
    )
    .render { (props, ctx, adding, showDeleted, programsPot, _, _, table) =>
      import ctx.given

      val closeButton =
        props.onClose.fold(none)(cb =>
          Button(label = "Cancel",
                 icon = Icons.Close,
                 severity = Button.Severity.Danger,
                 onClick = cb
          ).small.compact.some
        )

      <.div(
        programsPot.render(programs =>
          React.Fragment(
            <.div(ExploreStyles.ProgramTable)(
              PrimeAutoHeightVirtualizedTable(
                table,
                estimateSize = _ => 32.toPx,
                striped = true,
                compact = Compact.Very,
                tableMod = ExploreStyles.ExploreTable |+| ExploreStyles.ExploreBorderTable,
                emptyMessage = "No programs available"
              )
            ),
            // Since we can't add a footer to the Dialog here, we pretend to be one.
            <.div(ExploreStyles.ProgramsPopupFauxFooter)(
              Button(
                label = "Program",
                icon = Icons.New,
                severity = Button.Severity.Success,
                disabled = adding.get.value,
                loading = adding.get.value,
                onClick = addProgram(programs, adding).runAsync
              ).small.compact,
              CheckboxView(id = "show-deleted".refined,
                           value = showDeleted.zoom(ShowDeleted.value.asLens),
                           label = "Show deleted"
              ),
              closeButton
            )
          )
        )
      )
    }
