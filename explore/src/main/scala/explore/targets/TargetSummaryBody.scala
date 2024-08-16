// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import explore.Icons
import explore.common.UserPreferencesQueries
import explore.common.UserPreferencesQueries.TableStore
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Focused
import explore.model.Observation
import explore.model.ProgramSummaries
import explore.model.TargetList
import explore.model.enums.AppTab
import explore.model.enums.TableId
import explore.syntax.ui.*
import explore.undo.UndoContext
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.NewType
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.ConfirmDialog
import lucuma.react.primereact.DialogPosition
import lucuma.react.primereact.PrimeStyles
import lucuma.react.resizeDetector.hooks.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.schemas.model.TargetWithId
import lucuma.typed.tanstackVirtualCore as rawVirtual
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.table.hooks.*
import org.scalajs.dom.File as DOMFile

import scala.collection.immutable.SortedSet
import monocle.Focus
import explore.components.ColumnSelectorState

case class TargetSummaryTileState(
  filesToImport:   List[DOMFile],
  table:           ColumnSelectorState[TargetWithId, Nothing],
  deletingTargets: DeletingTargets
)

object DeletingTargets extends NewType[Boolean]
type DeletingTargets = DeletingTargets.Type

object TargetSummaryTileState:
  val filesToImport   = Focus[TargetSummaryTileState](_.filesToImport)
  val table           = Focus[TargetSummaryTileState](_.table)
  val deletingTargets = Focus[TargetSummaryTileState](_.deletingTargets)

case class TargetSummaryBody(
  userId:                  Option[User.Id],
  programId:               Program.Id,
  targets:                 View[TargetList],
  targetObservations:      Map[Target.Id, SortedSet[Observation.Id]],
  calibrationObservations: Set[Observation.Id],
  selectObservation:       (Observation.Id, Target.Id) => Callback,
  selectTargetOrSummary:   Option[Target.Id] => Callback,
  selectedTargetIds:       View[List[Target.Id]],
  undoCtx:                 UndoContext[ProgramSummaries],
  readonly:                Boolean
)(val state: View[TargetSummaryTileState])
    extends ReactFnProps(TargetSummaryBody.component):
  val filesToImport   = state.zoom(TargetSummaryTileState.filesToImport)
  val table           = state.zoom(TargetSummaryTileState.table)
  val deletingTargets = state.zoom(TargetSummaryTileState.deletingTargets)

object TargetSummaryBody:
  private type Props = TargetSummaryBody

  private val ColDef = ColumnDef[TargetWithId]

  private val IdColumnId: ColumnId           = ColumnId("id")
  private val CountColumnId: ColumnId        = ColumnId("count")
  private val ObservationsColumnId: ColumnId = ColumnId("observations")

  private object IsImportOpen extends NewType[Boolean]

  private val columnClasses: Map[ColumnId, Css] = Map(
    IdColumnId                 -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryId),
    TargetColumns.TypeColumnId -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryType),
    TargetColumns.NameColumnId -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryName)
  )

  val ColNames: Map[ColumnId, String] =
    TargetColumns.AllColNames ++ Map(
      IdColumnId           -> "Id",
      CountColumnId        -> "Count",
      ObservationsColumnId -> "Observations"
    )

  private val ScrollOptions =
    rawVirtual.mod
      .ScrollToOptions()
      .setBehavior(rawVirtual.mod.ScrollBehavior.auto)
      .setAlign(rawVirtual.mod.ScrollAlignment.center)

  private given Reusability[Map[Target.Id, SortedSet[Observation.Id]]] = Reusability.map

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useMemoBy((_, _) => ()): (props, ctx) => // cols
        _ =>
          def column[V](id: ColumnId, accessor: TargetWithId => V) =
            ColDef(id, row => accessor(row), ColNames(id))

          def obsUrl(targetId: Target.Id, obsId: Observation.Id): String =
            ctx.pageUrl(AppTab.Targets, props.programId, Focused.singleObs(obsId, targetId.some))

          List(
            ColDef(
              IdColumnId,
              _.id,
              "id",
              _.value.toString
            ).sortable
          ) ++
            TargetColumns.Builder.ForProgram(ColDef, _.target.some).AllColumns ++
            List(
              column(
                CountColumnId,
                x => props.targetObservations.get(x.id).map(_.size).orEmpty
              ) // TODO Right align
                .setCell(_.value.toString),
              column(
                ObservationsColumnId,
                x => (x.id, props.targetObservations.get(x.id).orEmpty.toList)
              )
                .setCell(cell =>
                  val (tid, obsIds) = cell.value
                  <.span(
                    obsIds
                      .map(obsId =>
                        <.a(
                          ^.href := obsUrl(tid, obsId),
                          ^.onClick ==> (e =>
                            e.preventDefaultCB *>
                              props.selectObservation(obsId, cell.row.original.id)
                          ),
                          obsId.show
                        )
                      )
                      .mkReactFragment(", ")
                  )
                )
                .setEnableSorting(false)
            )
      .useMemoBy((props, _, _) => // rows
        (props.targets.get, props.targetObservations, props.calibrationObservations)
      ): (_, _, _) =>
        (targets, targetObservations, calibrationObservations) =>
          def isCalibrationTarget(targetId: Target.Id): Boolean =
            targetObservations.get(targetId).exists(_.forall(calibrationObservations.contains_))

          targets.toList
            .filterNot((id, _) => isCalibrationTarget(id))
            .map((id, target) => TargetWithId(id, target))
      .useReactTableWithStateStoreBy: (props, ctx, cols, rows) =>
        import ctx.given

        def targetIds2RowSelection: List[Target.Id] => RowSelection = targetIds =>
          RowSelection(
            targetIds.map(targetId => RowId(targetId.toString) -> true).toMap
          )

        def rowSelection2TargetIds: RowSelection => List[Target.Id] = selection =>
          selection.value
            .filter(_._2)
            .keys
            .toList
            .map(rowId => Target.Id.parse(rowId.value))
            .flattenOption

        TableOptionsWithStateStore(
          TableOptions(
            cols,
            rows,
            getRowId = (row, _, _) => RowId(row.id.toString),
            enableSorting = true,
            enableColumnResizing = true,
            enableMultiRowSelection = true,
            columnResizeMode = ColumnResizeMode.OnChange,
            state = PartialTableState(
              rowSelection = targetIds2RowSelection(props.selectedTargetIds.get)
            ),
            onRowSelectionChange = _ match
              case Updater.Set(selection) =>
                props.selectedTargetIds.set(rowSelection2TargetIds(selection))
              case Updater.Mod(f)         =>
                props.selectedTargetIds.mod(targetIds =>
                  rowSelection2TargetIds(f(targetIds2RowSelection(targetIds)))
                )
            ,
            initialState = TableState(
              columnVisibility = TargetColumns.DefaultVisibility
            )
          ),
          TableStore(props.userId, TableId.TargetsSummary, cols)
        )
      .useEffectOnMountBy((p, _, _, _, table) => p.table.set(ColumnSelectorState(table.some)))
      // Copy the selection upstream
      .useEffectWithDepsBy((_, _, _, _, table) =>
        table.getSelectedRowModel().rows.toList.map(_.original.id)
      ): (props, ctx, _, _, _) =>
        ids =>
          props.selectedTargetIds.set(ids) >>
            ids.headOption
              .filter(_ => ids.length === 1) // Only if there's just 1 target selected
              .map(targetId =>
                ctx.pushPage(AppTab.Targets, props.programId, Focused.target(targetId))
              )
              .getOrElse(ctx.pushPage(AppTab.Targets, props.programId, Focused.None))
      .useRef(none[HTMLTableVirtualizer])
      .useResizeDetector()
      .useEffectWithDepsBy((props, _, _, _, _, _, resizer) =>
        (props.selectedTargetIds.get.headOption, resizer)
      ): (_, _, _, _, table, virtualizerRef, _) =>
        (selectedTargetIds, _) =>
          selectedTargetIds.foldMap(selectedHead =>
            virtualizerRef.get.flatMap(refOpt =>
              val selectedIdStr = selectedHead.toString
              Callback(
                for
                  virtualizer <- refOpt
                  idx         <- table
                                   .getRowModel()
                                   .flatRows
                                   .indexWhere(_.id.value === selectedIdStr)
                                   .some
                                   .filterNot(_ == -1)
                yield virtualizer.scrollToIndex(idx + 1, ScrollOptions)
              )
            )
          )
      .render: (props, _, _, _, table, virtualizerRef, resizer) =>

        val selectedRows = table.getSelectedRowModel().rows.toList

        PrimeAutoHeightVirtualizedTable(
          table,
          _ => 32.toPx,
          striped = true,
          compact = Compact.Very,
          innerContainerMod = ^.width := "100%",
          containerRef = resizer.ref,
          tableMod = ExploreStyles.ExploreTable |+| ExploreStyles.ExploreSelectableTable,
          headerCellMod = headerCell =>
            columnClasses
              .get(headerCell.column.id)
              .orEmpty |+| ExploreStyles.StickyHeader,
          rowMod = row =>
            TagMod(
              ExploreStyles.TableRowSelected.when_(row.getIsSelected()),
              ^.onClick ==> { (e: ReactMouseEvent) =>
                val isShiftPressed   = e.shiftKey
                val isCmdCtrlPressed = e.metaKey || e.ctrlKey

                // If cmd is pressed add to the selection
                table.toggleAllRowsSelected(false).unless(isCmdCtrlPressed) *> {
                  if (isShiftPressed && selectedRows.nonEmpty) {
                    // If shift is pressed extend
                    val allRows        =
                      table.getRowModel().rows.toList.zipWithIndex
                    val currentId      = row.id
                    // selectedRow is not empty, these won't fail
                    val firstId        = selectedRows.head.id
                    val lastId         = selectedRows.last.id
                    val indexOfCurrent = allRows.indexWhere(_._1.id == currentId)
                    val indexOfFirst   = allRows.indexWhere(_._1.id == firstId)
                    val indexOfLast    = allRows.indexWhere(_._1.id == lastId)
                    if (indexOfCurrent =!= -1 && indexOfFirst =!= -1 && indexOfLast =!= -1) {
                      if (indexOfCurrent < indexOfFirst) {
                        table.setRowSelection(
                          RowSelection(
                            (firstId -> true) :: allRows
                              .slice(indexOfCurrent, indexOfFirst)
                              .map { case (row, _) => row.id -> true }*
                          )
                        )
                      } else {
                        table.setRowSelection(
                          RowSelection(
                            (currentId -> true) :: allRows
                              .slice(indexOfLast, indexOfCurrent)
                              .map { case (row, _) => row.id -> true }*
                          )
                        )
                      }
                    } else Callback.empty
                  } else row.toggleSelected()
                }
              }
            ),
          cellMod = cell => columnClasses.get(cell.column.id).orEmpty,
          virtualizerRef = virtualizerRef,
          emptyMessage = <.div("No targets present")
          // workaround to redraw when files are imported
        ).withKey(s"summary-table-${props.filesToImport.get.size}")

case class TargetSummaryTitle(
  programId:             Program.Id,
  targets:               View[TargetList],
  selectTargetOrSummary: Option[Target.Id] => Callback,
  selectedTargetIds:     View[List[Target.Id]],
  undoCtx:               UndoContext[ProgramSummaries],
  readonly:              Boolean
)(val state: View[TargetSummaryTileState])
    extends ReactFnProps(TargetSummaryTitle.component) {
  val filesToImport   = state.zoom(TargetSummaryTileState.filesToImport)
  val table           = state.zoom(TargetSummaryTileState.table)
  val deletingTargets = state.zoom(TargetSummaryTileState.deletingTargets)
}

object TargetSummaryTitle:
  private type Props = TargetSummaryTitle

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .render: (props, ctx) =>
        import ctx.given

        props.table.get.table.map { table =>
          val selectedRows    = table.getSelectedRowModel().rows.toList
          val selectedRowsIds = selectedRows.map(_.original.id)

          def onTextChange(e: ReactEventFromInput): Callback =
            val files = e.target.files.toList
            // set value to null so we can reuse the import button
            (Callback(e.target.value = null) *> props.filesToImport.set(files))
              .when_(files.nonEmpty)

          def deleteSelected: Callback =
            ConfirmDialog.confirmDialog(
              message = <.div(s"This action will delete ${selectedRows.length} targets."),
              header = "Targets delete",
              acceptLabel = "Yes, delete",
              position = DialogPosition.Top,
              accept = props.targets
                .mod(_.filter((id, _) => !selectedRowsIds.contains(id))) *>
                props.table.get.table.map(_.toggleAllRowsSelected(false)).getOrEmpty *>
                TargetAddDeleteActions
                  .deleteTargets(
                    selectedRowsIds,
                    props.programId,
                    props.selectTargetOrSummary(none).toAsync,
                    ToastCtx[IO].showToast(_)
                  )
                  .set(props.undoCtx)(selectedRowsIds.map(_ => none))
                  .toAsync
                  .switching(props.deletingTargets.async, DeletingTargets(_))
                  .runAsyncAndForget,
              acceptClass = PrimeStyles.ButtonSmall,
              rejectClass = PrimeStyles.ButtonSmall,
              icon = Icons.SkullCrossBones(^.color.red)
            )

          React.Fragment(
            if (props.readonly) EmptyVdom
            else
              <.div(
                ExploreStyles.TableSelectionToolbar,
                HelpIcon("target/main/target-import.md".refined),
                <.label(
                  PrimeStyles.Component |+| PrimeStyles.Button |+| LucumaPrimeStyles.Compact |+| ExploreStyles.FileUpload,
                  ^.htmlFor := "target-import",
                  Icons.FileArrowUp
                ),
                <.input(
                  ^.tpe     := "file",
                  ^.onChange ==> onTextChange,
                  ^.id      := "target-import",
                  ^.name    := "file",
                  ^.accept  := ".csv"
                ),
                TargetImportPopup(props.programId, props.filesToImport),
                Button(
                  size = Button.Size.Small,
                  icon = Icons.CheckDouble,
                  label = "All",
                  onClick = table.toggleAllRowsSelected(true)
                ).compact,
                Button(
                  size = Button.Size.Small,
                  icon = Icons.SquareXMark,
                  label = "None",
                  onClick = table.toggleAllRowsSelected(false)
                ).compact,
                Button(
                  size = Button.Size.Small,
                  icon = Icons.Trash,
                  disabled = props.deletingTargets.get.value,
                  loading = props.deletingTargets.get.value,
                  onClick = deleteSelected
                ).compact.when(selectedRows.nonEmpty)
              ),
            <.span(ExploreStyles.TitleSelectColumns)(
              ColumnSelector(table, TargetSummaryBody.ColNames, ExploreStyles.SelectColumns)
            )
          )
        }
