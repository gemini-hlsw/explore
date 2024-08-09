// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.react.*
import crystal.react.reuse.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.*
import explore.EditableLabel
import explore.Icons
import explore.common.ProgramQueries
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ProgramInfo
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.Reusability
import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.ObservationDB
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import org.typelevel.log4cats.Logger

case class ProgramTable(
  currentProgramId: Option[Program.Id],
  programInfos:     List[View[ProgramInfo]],
  selectProgram:    Program.Id => Callback,
  isRequired:       Boolean,
  onClose:          Option[Callback],
  onLogout:         Option[IO[Unit]]
) extends ReactFnProps(ProgramTable.component)

object ProgramTable:
  private type Props = ProgramTable

  private case class TableMeta(currentProgramId: Option[Program.Id], programCount: Int)

  private val ColDef = ColumnDef.WithTableMeta[View[ProgramInfo], TableMeta]

  private given Reusability[List[View[ProgramInfo]]] = Reusability.by(_.map(_.get))

  private def deleteProgram(pinf: View[ProgramInfo])(using
    FetchClient[IO, ObservationDB]
  ): IO[Unit] =
    pinf.zoom(ProgramInfo.deleted).set(true).toAsync >>
      ProgramQueries.deleteProgram[IO](pinf.get.id)

  private def undeleteProgram(pinf: View[ProgramInfo])(using
    FetchClient[IO, ObservationDB],
    Logger[IO]
  ): IO[Unit] =
    pinf.zoom(ProgramInfo.deleted).set(false).toAsync >>
      ProgramQueries.undeleteProgram[IO](pinf.get.id)

  private def onModName(pinf: ProgramInfo)(using
    FetchClient[IO, ObservationDB],
    Logger[IO]
  ): Callback =
    ProgramQueries.updateProgramName[IO](pinf.id, pinf.name).runAsync

  private val ActionsColumnId: ColumnId = ColumnId("actions")
  private val IdColumnId: ColumnId      = ColumnId("id")
  private val NameColumnId: ColumnId    = ColumnId("name")

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useMemoBy((_, _) => ()): (props, ctx) => // Columns
      _ =>
        import ctx.given

        List(
          ColDef(
            ActionsColumnId,
            identity[View[ProgramInfo]],
            "Actions",
            cell = cell =>
              cell.table.options.meta.map: meta =>
                val programId = cell.value.get.id
                val isDeleted = cell.value.get.deleted

                <.div(
                  Button(
                    label = "Select",
                    icon = Icons.Checkmark,
                    severity = Button.Severity.Secondary,
                    disabled = meta.currentProgramId.exists(_ === programId),
                    onClick = props.selectProgram(programId)
                  ).compact.mini.unless(cell.row.original.get.deleted),
                  Button(
                    icon = Icons.Trash,
                    severity = Button.Severity.Secondary,
                    disabled =
                      meta.currentProgramId.exists(_ === programId) || meta.programCount < 2,
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
            ,
            size = 100.toPx,
            minSize = 100.toPx,
            maxSize = 120.toPx
          ).sortableBy(_.get.deleted),
          ColDef(
            IdColumnId,
            _.get.id,
            "Id",
            _.value.toString,
            size = 60.toPx,
            minSize = 60.toPx,
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
    .useMemoBy((props, _, _) => props.programInfos)((_, _, _) => identity) // Rows
    .useReactTableBy: (props, _, cols, rows) =>
      TableOptions(
        cols,
        rows,
        enableSorting = true,
        enableColumnResizing = false,
        meta = TableMeta(props.currentProgramId, props.programInfos.size)
      )
    .render: (props, ctx, _, _, table) =>
      PrimeAutoHeightVirtualizedTable(
        table,
        estimateSize = _ => 32.toPx,
        striped = true,
        compact = Compact.Very,
        tableMod = ExploreStyles.ExploreTable |+| ExploreStyles.ExploreBorderTable,
        emptyMessage = "No programs available"
      )
