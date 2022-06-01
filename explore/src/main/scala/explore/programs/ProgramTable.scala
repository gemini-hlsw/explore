// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.Order._
import cats.effect.IO
import cats.syntax.all._
import crystal.react.View
import crystal.react.implicits._
import crystal.react.reuse._
import explore.EditableLabel
import explore.Icons
import explore.common.ProgramQueries
import explore.common.ProgramQueries.ProgramInfo
import explore.components.ui.ExploreStyles
import explore.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Program
import lucuma.ui.reusability._
import react.common.ReactFnProps
import react.common.implicits._
import react.semanticui.collections.table.TableCompact
import react.semanticui.collections.table._
import react.semanticui.elements.button._
import react.semanticui.shorthand._
import react.semanticui.sizes._
import reactST.reactTable._

final case class ProgramTable(
  currentProgramId: Option[Program.Id],
  programs:         View[List[ProgramInfo]],
  showDeleted:      Boolean,
  selectProgram:    Program.Id => Callback
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[ProgramTable](ProgramTable.component)

object ProgramTable {
  type Props = ProgramTable

  protected val ProgsTableDef = TableDef[View[ProgramInfo]].withSortBy.withFlexLayout

  protected val ProgsTable = new SUITableVirtuoso(ProgsTableDef)

  protected def deleteProgram(pinf: View[ProgramInfo])(implicit ctx: AppContextIO): IO[Unit] =
    pinf.zoom(ProgramInfo.deleted).set(true).to[IO] >>
      ProgramQueries.deleteProgram[IO](pinf.get.id)

  protected def undeleteProgram(pinf: View[ProgramInfo])(implicit
    ctx:                              AppContextIO
  ): IO[Unit] =
    pinf.zoom(ProgramInfo.deleted).set(false).to[IO] >>
      ProgramQueries.undeleteProgram[IO](pinf.get.id)

  protected def onModName(pinf: ProgramInfo)(implicit ctx: AppContextIO): Callback =
    ProgramQueries.updateProgramName[IO](pinf.id, pinf.name).runAsync

  val component = ScalaFnComponent
    .withHooks[Props]
    // Columns
    .useMemoBy(props => (props.currentProgramId, props.programs.get.length)) { props => deps =>
      val (currentProgramId, programCount) = deps
      implicit val ctx                     = props.ctx

      List(
        ProgsTableDef
          .Column("actions", identity[View[ProgramInfo]] _)
          .setHeader("Actions")
          .setCell { cell =>
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
              ).unless(isDeleted),
              Button(
                content = "Undelete",
                size = Mini,
                compact = true,
                icon = Icons.TrashUndo,
                onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) =>
                  e.preventDefaultCB >>
                    e.stopPropagationCB >>
                    undeleteProgram(cell.value).runAsync
              ).when(isDeleted)
            )
          }
          .setWidth(0)
          .setMinWidth(120)
          .setMaxWidth(0)
          .setSortByFn(_.get.deleted),
        ProgsTableDef
          .Column("id", _.get.id)
          .setHeader("Id")
          .setCell(_.value.toString)
          .setWidth(0)
          .setMinWidth(50)
          .setMaxWidth(0)
          .setSortByAuto,
        ProgsTableDef
          .Column("name", _.withOnMod(onModName).zoom(ProgramInfo.name))
          .setHeader("Name")
          .setCell(cell =>
            EditableLabel.fromView(
              value = cell.value,
              addButtonLabel = ("Add program name": VdomNode).reuseAlways,
              textClass = ExploreStyles.ProgramName,
              inputClass = ExploreStyles.ProgramNameInput,
              editButtonClass = ExploreStyles.BlendedButton |+| ExploreStyles.ProgramNameEdit,
              deleteButtonClass = ExploreStyles.BlendedButton |+| ExploreStyles.ProgramNameDelete
            )
          )
          .setWidth(1)
          .setMinWidth(200)
          .setMaxWidth(1000)
          .setSortByFn(_.get.foldMap(_.value))
      )
    }
    // Rows
    .useMemoBy((props, _) => (props.programs.get, props.showDeleted)) { (props, _) => _ =>
      props.programs.toListOfViews
        .filter(vpi => props.showDeleted || !vpi.get.deleted)
        .sortBy(_.get.id)
    }
    .useTableBy((_, cols, rows) =>
      ProgsTableDef(
        cols,
        rows,
        ((options: ProgsTableDef.OptionsType) => options.setAutoResetSortBy(false)).reuseAlways
      )
    )
    .render((_, _, _, tableInstance) =>
      <.div(
        ExploreStyles.ProgramTable,
        ProgsTable.Component(
          table =
            Table(celled = true, striped = true, unstackable = true, compact = TableCompact.Very),
          header = TableHeader(),
          emptyMessage = "No programs available"
        )(tableInstance)
      )
    )
}
