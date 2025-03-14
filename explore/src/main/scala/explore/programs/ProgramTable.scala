// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.react.*
import crystal.react.reuse.*
import explore.*
import explore.common.ProgramQueries
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ProgramInfo
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.hooks.Hooks.UseRef
import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.User
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
  userId:           User.Id,
  isStaff:          Boolean,
  programInfos:     List[View[ProgramInfo]],
  selectProgram:    Program.Id => Callback,
  isRequired:       Boolean,
  onClose:          Option[Callback],
  newProgramId:     Option[Program.Id],
  virtualizerRef:   UseRef[Option[HTMLTableVirtualizer]]
) extends ReactFnProps(ProgramTable.component)

object ProgramTable:
  private type Props = ProgramTable

  private case class TableMeta(
    currentProgramId: Option[Program.Id],
    userId:           User.Id,
    isStaff:          Boolean,
    newProgramId:     Option[Program.Id],
    programCount:     Int
  )

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

  private val ActionsColumnId: ColumnId   = ColumnId("actions")
  private val IdColumnId: ColumnId        = ColumnId("id")
  private val ReferenceColumnId: ColumnId = ColumnId("reference")
  private val StatusColumnId: ColumnId    = ColumnId("status")
  private val PiColumnId: ColumnId        = ColumnId("pi")
  private val NameColumnId: ColumnId      = ColumnId("name")

  private val component = ScalaFnComponent[Props](props =>
    for {
      ctx   <- useContext(AppContext.ctx)
      cols  <-
        useMemo(()): _ =>
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
                  val canDelete = cell.value.get.canDelete(meta.userId, meta.isStaff)

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
                      disabled = meta.currentProgramId.exists(
                        _ === programId
                      ) || meta.programCount < 2,
                      onClick = deleteProgram(cell.value).runAsync,
                      tooltip = "Delete program"
                    ).mini.compact.unless(isDeleted || !canDelete),
                    Button(
                      label = "Undelete",
                      icon = Icons.TrashUndo,
                      severity = Button.Severity.Secondary,
                      onClick = undeleteProgram(cell.value).runAsync,
                      tooltip = "Undelete program"
                    ).mini.compact.when(isDeleted && canDelete)
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
              ReferenceColumnId,
              v =>
                v.get.programReference
                  .map(_.label)
                  .orElse(v.get.proposalReference.map(_.label))
                  .orEmpty,
              "Reference"
            ).sortable,
            ColDef(
              StatusColumnId,
              v =>
                if (v.get.hasProposal)
                  v.get.proposalStatus.name
                else "",
              "Status"
            ).sortable,
            ColDef(
              PiColumnId,
              _.get.pi.map(_.lastName).orEmpty,
              "PI"
            ).sortable,
            ColDef(
              NameColumnId,
              identity[View[ProgramInfo]],
              "Name",
              cell =>
                cell.table.options.meta.map: meta =>
                  val pid          = cell.row.original.get.id
                  val isNewProgram = meta.newProgramId.contains(pid)
                  // for a new program, we'll select it once it is named.
                  val selectCB     =
                    if (isNewProgram) props.selectProgram(pid) else Callback.empty
                  if (cell.value.get.canRename(meta.userId, meta.isStaff))
                    val nameView =
                      cell.value
                        .withOnMod(pinf => onModName(pinf) >> selectCB)
                        .zoom(ProgramInfo.name)
                    EditableLabel
                      .fromView(
                        value = nameView,
                        forceEditing = isNewProgram,
                        addButtonLabel = ("Add program name": VdomNode).reuseAlways,
                        textClass = ExploreStyles.ProgramName,
                        inputClass = ExploreStyles.ProgramNameInput,
                        editButtonTooltip = "Edit program name".some,
                        deleteButtonTooltip = "Delete program name".some,
                        okButtonTooltip = "Accept".some,
                        discardButtonTooltip = "Discard".some
                      ): VdomNode
                  else <.span(cell.value.get.name.map(_.value))
              ,
              size = 400.toPx,
              minSize = 200.toPx,
              maxSize = 1000.toPx
            ).sortableBy(_.get.name.foldMap(_.value))
          )
      rows  <- useMemo(props.programInfos)(identity)
      table <- useReactTable(
                 TableOptions(
                   cols,
                   rows,
                   enableSorting = true,
                   enableColumnResizing = false,
                   meta = TableMeta(props.currentProgramId,
                                    props.userId,
                                    props.isStaff,
                                    props.newProgramId,
                                    props.programInfos.size
                   )
                 )
               )
    } yield PrimeAutoHeightVirtualizedTable(
      table,
      estimateSize = _ => 32.toPx,
      striped = true,
      compact = Compact.Very,
      tableMod = ExploreStyles.ExploreTable |+| ExploreStyles.ExploreBorderTable,
      virtualizerRef = props.virtualizerRef,
      emptyMessage = "No programs available"
    )
  )
