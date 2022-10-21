// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.ObservationDB
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import org.typelevel.log4cats.Logger
import queries.common.ProgramQueriesGQL.*
import react.common.ReactFnProps
import react.floatingui.Placement
import react.floatingui.syntax.*
import react.semanticui.collections.table.TableCompact
import react.semanticui.collections.table.*
import react.semanticui.elements.button.*
import react.semanticui.modules.checkbox.Checkbox
import react.semanticui.shorthand.*
import react.semanticui.sizes.*
import reactST.{tanstackTableCore => raw}

case class ProgramTable(
  currentProgramId: Option[Program.Id],
  selectProgram:    Program.Id => Callback,
  isRequired:       Boolean
) extends ReactFnProps(ProgramTable.component)

object ProgramTable:
  private type Props = ProgramTable

  private val ColDef = ColumnDef[View[ProgramInfo]]

  private def addProgram(programs: View[List[ProgramInfo]], adding: View[Boolean])(using
    TransactionalClient[IO, ObservationDB],
    Logger[IO]
  ): IO[Unit] =
    adding.async.set(true) >>
      ProgramQueries
        .createProgram[IO](none)
        .flatMap(pi => programs.async.mod(_ :+ pi))
        .guarantee(adding.async.set(false))

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
    .useStateView(false) // Adding new program
    .useStateView(false) // Show deleted
    .useStreamResourceViewBy((_, _, _, showDeleted) => showDeleted.get) {
      (props, ctx, _, _) => showDeleted =>
        import ctx.given

        ProgramsQuery
          .query(includeDeleted = showDeleted)
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
                content = "Select",
                size = Mini,
                compact = true,
                icon = Icons.Checkmark,
                disabled = currentProgramId.exists(_ === programId),
                onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) =>
                  e.preventDefaultCB >>
                    e.stopPropagationCB >>
                    props.selectProgram(programId)
              ).unless(cell.row.original.get.deleted),
              <.span(
                Button(
                  size = Mini,
                  compact = true,
                  icon = Icons.Trash,
                  // can't delete the current or last one
                  disabled = currentProgramId.exists(_ === programId) || programCount < 2,
                  onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) =>
                    e.preventDefaultCB >>
                      e.stopPropagationCB >>
                      deleteProgram(cell.value).runAsync
                )
              ).withTooltip(tooltip = "Delete program", placement = Placement.Right)
                .unless(isDeleted),
              <.span(
                Button(
                  content = "Undelete",
                  size = Mini,
                  compact = true,
                  icon = Icons.TrashUndo,
                  onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) =>
                    e.preventDefaultCB >>
                      e.stopPropagationCB >>
                      undeleteProgram(cell.value).runAsync
                )
              ).withTooltip(
                tooltip = "Undelete program",
                placement = Placement.Right
              ).when(isDeleted)
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
                editButtonClass = ExploreStyles.BlendedButton |+| ExploreStyles.ProgramNameEdit,
                editButtonTooltip = "Edit program name".some,
                deleteButtonClass = ExploreStyles.BlendedButton |+| ExploreStyles.ProgramNameDelete,
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
            .filter(vpi => showDeleted || !vpi.get.deleted)
            .sortBy(_.get.id)
        )
        .orEmpty
    }
    .useReactTableBy((_, _, _, _, _, cols, rows) =>
      TableOptions(cols, rows, enableSorting = true, enableColumnResizing = false)
    )
    .render { (props, ctx, adding, showDeleted, programsPot, _, _, table) =>
      import ctx.given

      <.div(ExploreStyles.ProgramTable)(
        programsPot.render(programs =>
          React.Fragment(
            PrimeAutoHeightVirtualizedTable(
              table,
              estimateRowHeight = _ => 32.toPx,
              striped = true,
              compact = Compact.Very,
              tableMod = ExploreStyles.ExploreTable |+| ExploreStyles.ExploreBorderTable,
              emptyMessage = "No programs available"
            ),
            <.div(ExploreStyles.ProgramsPopupAdditionalActions)(
              Button(
                clazz = ExploreStyles.ProgramAdd,
                compact = true,
                positive = true,
                icon = Icons.New,
                content = "Program",
                disabled = adding.get,
                loading = adding.get,
                onClick = addProgram(programs, adding).runAsync
              ),
              Checkbox(
                label = "Show deleted",
                checked = showDeleted.get,
                onChange = showDeleted.set
              )
            )
          )
        )
      )
    }
