// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.Order._
import cats.effect.IO
import cats.syntax.all._
import crystal.react.View
import crystal.react.hooks._
import crystal.react.implicits._
import crystal.react.reuse._
import explore.EditableLabel
import explore.Icons
import explore.common.ProgramQueries
import explore.common.ProgramQueries.ProgramInfo
import explore.common.ProgramQueries._
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.Focused
import explore.model.enums.AppTab
import explore.syntax.ui.given
import explore.utils._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.VdomNode
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Program
import lucuma.ui.reusability._
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import queries.common.ProgramQueriesGQL._
import react.common.ReactFnProps
import react.semanticui.collections.table.TableCompact
import react.semanticui.collections.table._
import react.semanticui.elements.button._
import react.semanticui.modules.checkbox.Checkbox
import react.semanticui.shorthand._
import react.semanticui.sizes._
import reactST.reactTable._

final case class ProgramTable(
  currentProgramId: Option[Program.Id],
  selectProgram:    Program.Id => Callback,
  isRequired:       Boolean
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[ProgramTable](ProgramTable.component)

object ProgramTable {
  type Props = ProgramTable

  protected val ProgsTableDef = TableDef[View[ProgramInfo]].withSortBy.withFlexLayout

  protected val ProgsTable = new SUITableVirtuoso(ProgsTableDef)

  protected def addProgram(programs: View[List[ProgramInfo]], adding: View[Boolean])(implicit
    ctx:                             AppContextIO
  ): IO[Unit] =
    adding.async.set(true) >>
      ProgramQueries
        .createProgram[IO](none)
        .flatMap(pi => programs.async.mod(_ :+ pi))
        .guarantee(adding.async.set(false))

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

  protected def onNewData(
    isRequired:   Boolean,
    programs:     List[ProgramInfo]
  )(implicit ctx: AppContextIO): IO[Unit] =
    (isRequired, programs) match {
      case (true, head :: Nil) => ctx.replacePage(AppTab.Overview, head.id, Focused.None).to[IO]
      case _                   => IO.unit
    }

  val component = ScalaFnComponent
    .withHooks[Props]
    .useStateView(false) // Adding new program
    .useStateView(false) // Show deleted
    .useStreamResourceViewBy((_, _, showDeleted) => showDeleted.get) {
      (props, _, _) => showDeleted =>
        implicit val ctx = props.ctx

        ProgramsQuery
          .query(includeDeleted = showDeleted)
          .map(ProgramsQuery.Data.asProgramInfoList)
          .flatTap(programs => onNewData(props.isRequired, programs))
          .reRunOnResourceSignals(ProgramEditSubscription.subscribe[IO]())
    }
    // Columns
    .useMemoBy((props, _, _, programs) =>
      (props.currentProgramId, programs.toOption.map(_.get.length).orEmpty)
    ) { (props, _, _, _) =>
      { case (currentProgramId, programCount) =>
        implicit val ctx = props.ctx

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
    }
    // Rows
    .useMemoBy((_, _, showDeleted, programs, _) =>
      (programs.toOption.map(_.reuseByValue), showDeleted.get)
    ) { (_, _, _, _, _) =>
      { case (programs, showDeleted) =>
        programs
          .map(
            _.value.toListOfViews
              .filter(vpi => showDeleted || !vpi.get.deleted)
              .sortBy(_.get.id)
          )
          .orEmpty
      }
    }
    .useTableBy((_, _, _, _, cols, rows) =>
      ProgsTableDef(
        cols,
        rows,
        ((options: ProgsTableDef.OptionsType) => options.setAutoResetSortBy(false)).reuseAlways
      )
    )
    .render { (props, adding, showDeleted, programsPot, _, _, tableInstance) =>
      implicit val ctx = props.ctx

      <.div(ExploreStyles.ProgramTable)(
        programsPot.render(programs =>
          React.Fragment(
            ProgsTable.Component(
              table = Table(
                celled = true,
                striped = true,
                unstackable = true,
                compact = TableCompact.Very
              ),
              header = TableHeader(),
              emptyMessage = "No programs available"
            )(tableInstance),
            <.div(
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
}
