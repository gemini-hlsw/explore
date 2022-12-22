// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order.*
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import crystal.react.reuse.*
import explore.Icons
import explore.common.AsterismQueries.*
import explore.common.UserPreferencesQueries.TableStore
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Focused
import explore.model.TargetWithIdAndObs
import explore.model.TargetWithObs
import explore.model.enums.AppTab
import explore.model.enums.TableId
import explore.syntax.ui.*
import explore.undo.UndoContext
import explore.utils.*
import fs2.dom
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Band
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.NewType
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.ui.primereact.*
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import org.scalablytyped.runtime.StringDictionary
import org.scalajs.dom.{File => DOMFile}
import react.common.Css
import react.common.ReactFnProps
import react.hotkeys.*
import react.primereact.Button
import react.primereact.ConfirmDialog
import react.primereact.DialogPosition
import react.primereact.PrimeStyles
import react.primereact.ToastRef
import reactST.{tanstackTableCore => raw}

import scala.collection.immutable.SortedSet

import scalajs.js.JSConverters.*

case class TargetSummaryTable(
  userId:                Option[User.Id],
  programId:             Program.Id,
  targets:               View[TargetWithObsList],
  selectObservation:     (Observation.Id, Target.Id) => Callback,
  selectTargetOrSummary: Option[Target.Id] => Callback,
  renderInTitle:         Tile.RenderInTitle,
  selectedTargetIds:     View[List[Target.Id]],
  undoCtx:               UndoContext[AsterismGroupsWithObs],
  toastRef:              ToastRef
) extends ReactFnProps(TargetSummaryTable.component)

object TargetSummaryTable extends TableHooks:
  private type Props = TargetSummaryTable

  private val ColDef = ColumnDef[TargetWithIdAndObs]

  private val IdColumnId: ColumnId           = ColumnId("id")
  private val CountColumnId: ColumnId        = ColumnId("count")
  private val ObservationsColumnId: ColumnId = ColumnId("observations")

  private object IsImportOpen extends NewType[Boolean]
  private type IsImportOpen = IsImportOpen.Type

  private object DeletingTargets extends NewType[Boolean]
  private type DeletingTargets = DeletingTargets.Type

  private val columnClasses: Map[ColumnId, Css] = Map(
    IdColumnId                 -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryId),
    TargetColumns.TypeColumnId -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryType |+| ExploreStyles.WithId),
    TargetColumns.NameColumnId -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryName |+| ExploreStyles.WithId)
  )

  private val colNames: Map[ColumnId, String] = TargetColumns.allColNames ++ Map(
    IdColumnId           -> "Id",
    CountColumnId        -> "Count",
    ObservationsColumnId -> "Observations"
  )

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // cols
      .useMemoBy((_, _) => ()) { (props, ctx) => _ =>
        def column[V](id: ColumnId, accessor: TargetWithIdAndObs => V) =
          ColDef(id, row => accessor(row), colNames(id))

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
          TargetColumns
            .BaseColumnBuilder(ColDef, _.target.some)
            .allColumns ++
          List(
            column(CountColumnId, _.obsIds.size) // TODO Right align
              .setCell(_.value.toString),
            column(ObservationsColumnId, x => (x.id, x.obsIds.toList))
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
      }
      // rows
      .useMemoBy((props, _, _) => props.targets.get)((_, _, _) =>
        _.toList.map((id, targetWithObs) => TargetWithIdAndObs(id, targetWithObs))
      )
      .useReactTableWithStateStoreBy((props, ctx, cols, rows) =>
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
      )
      // Files to be imported
      .useStateView(List.empty[DOMFile])
      .useStateView(DeletingTargets(false))
      // Copy the selection upstream
      .useEffectWithDepsBy((_, _, _, _, table, _, _) =>
        table.getSelectedRowModel().rows.toList.map(_.original.id)
      )((props, ctx, _, _, _, _, _) =>
        ids =>
          props.selectedTargetIds.set(ids) >>
            ids.headOption
              .filter(_ => ids.length === 1) // Only if there's just 1 target selected
              .map(targetId =>
                ctx.pushPage(AppTab.Targets, props.programId, Focused.target(targetId))
              )
              .getOrElse(ctx.pushPage(AppTab.Targets, props.programId, Focused.None))
      )
      .render((props, ctx, _, _, table, filesToImport, deletingTargets) =>
        import ctx.given

        val selectedRows    = table.getSelectedRowModel().rows.toList
        val selectedRowsIds = selectedRows.map(_.original.id)

        def deleteSelected: Callback =
          ConfirmDialog.confirmDialog(
            message = <.div(s"This action will delete ${selectedRows.length} targets."),
            header = "Targets delete",
            acceptLabel = "Yes, delete",
            position = DialogPosition.Top,
            accept = props.targets
              .mod(_.filter((id, _) => !selectedRowsIds.contains(id))) *>
              table.toggleAllRowsSelected(false) *>
              (deletingTargets.async.set(DeletingTargets(true)) >>
                TargetAddDeleteActions
                  .deleteTargets(
                    selectedRowsIds,
                    props.programId,
                    props.selectTargetOrSummary(none).to[IO],
                    props.toastRef.info(_).to[IO]
                  )
                  .set(props.undoCtx)(selectedRowsIds.map(_ => none[TargetWithObs]))
                  .to[IO]
                  .guarantee(
                    deletingTargets.async.set(DeletingTargets(false))
                  )).runAsyncAndForget,
            acceptClass = PrimeStyles.ButtonSmall,
            rejectClass = PrimeStyles.ButtonSmall,
            icon = Icons.SkullCrossBones.withColor("red")
          )

        def onTextChange(e: ReactEventFromInput): Callback =
          val files = e.target.files.toList
          // set value to null so we can reuse the import button
          (Callback(e.target.value = null) *> filesToImport.set(files)).when_(files.nonEmpty)

        React.Fragment(
          props.renderInTitle(
            React.Fragment(
              <.div(
                ExploreStyles.TableSelectionToolbar,
                HelpIcon("target/main/target-import.md".refined),
                <.label(^.cls     := "pl-compact p-component p-button p-fileupload",
                        ^.htmlFor := "target-import",
                        Icons.FileArrowUp
                ),
                <.input(^.tpe     := "file",
                        ^.onChange ==> onTextChange,
                        ^.id      := "target-import",
                        ^.name    := "file",
                        ^.accept  := ".csv"
                ),
                TargetImportPopup(props.programId, filesToImport),
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
                  disabled = deletingTargets.get.value,
                  loading = deletingTargets.get.value,
                  onClick = deleteSelected
                ).compact.when(selectedRows.nonEmpty),
                ConfirmDialog()
              ),
              <.span(ExploreStyles.TitleSelectColumns)(
                ColumnSelector(
                  table,
                  colNames,
                  ExploreStyles.SelectColumns
                )
              )
            )
          ),
          PrimeAutoHeightVirtualizedTable(
            table,
            _ => 32.toPx,
            striped = true,
            compact = Compact.Very,
            innerContainerMod = ^.width := "100%",
            tableMod = ExploreStyles.ExploreTable |+| ExploreStyles.ExploreSelectableTable,
            headerCellMod = headerCell =>
              columnClasses
                .get(ColumnId(headerCell.column.id))
                .orEmpty |+| ExploreStyles.StickyHeader,
            rowMod = row =>
              TagMod(
                ExploreStyles.TableRowSelected.when_(row.getIsSelected()),
                ^.onClick --> {
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
                      val indexOfCurrent = allRows.indexWhere(_._1.id === currentId)
                      val indexOfFirst   = allRows.indexWhere(_._1.id === firstId)
                      val indexOfLast    = allRows.indexWhere(_._1.id === lastId)
                      if (indexOfCurrent =!= -1 && indexOfFirst =!= -1 && indexOfLast =!= -1) {
                        if (indexOfCurrent < indexOfFirst) {
                          table.setRowSelection(
                            RowSelection(
                              (RowId(firstId) -> true) :: allRows
                                .slice(indexOfCurrent, indexOfFirst)
                                .map { case (row, _) => RowId(row.id) -> true }: _*
                            )
                          )
                        } else {
                          table.setRowSelection(
                            RowSelection(
                              (RowId(currentId) -> true) :: allRows
                                .slice(indexOfLast, indexOfCurrent)
                                .map { case (row, _) => RowId(row.id) -> true }: _*
                            )
                          )
                        }
                      } else Callback.empty
                    } else Callback(row.toggleSelected())
                  }
                }
              ),
            cellMod = cell => columnClasses.get(ColumnId(cell.column.id)).orEmpty,
            emptyMessage = <.div("No targets present")
            // workaround to redraw when files are imported
          ).withKey(s"summary-table-${filesToImport.get.size}")
        )
      )
