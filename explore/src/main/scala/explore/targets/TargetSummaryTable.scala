// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order.*
import cats.syntax.all.*
import crystal.react.View
import crystal.react.implicits.*
import crystal.react.reuse.*
import crystal.react.implicits.*
import explore.Icons
import explore.common.AsterismQueries.*
import explore.common.UserPreferencesQueries.TableStore
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Focused
import explore.model.TargetWithIdAndObs
import explore.model.enums.AppTab
import explore.model.enums.TableId
import explore.syntax.ui.*
import explore.utils.TableHooks
import explore.utils.TableOptionsWithStateStore
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Band
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.react.table.*
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import org.scalablytyped.runtime.StringDictionary
import react.common.Css
import react.common.ReactFnProps
import react.hotkeys.*
import react.primereact.Button
import react.primereact.ConfirmDialog
import react.primereact.DialogPosition
import react.primereact.PrimeStyles
import react.semanticui.collections.table.*
import reactST.react.reactStrings.I
import reactST.{tanstackTableCore => raw}
import fs2.dom

import scalajs.js.JSConverters.*
import cats.effect.IO

case class TargetSummaryTable(
  userId:            Option[User.Id],
  programId:         Program.Id,
  targets:           View[TargetWithObsList],
  selectObservation: (Observation.Id, Target.Id) => Callback,
  selectTarget:      Target.Id => Callback,
  renderInTitle:     Tile.RenderInTitle
) extends ReactFnProps(TargetSummaryTable.component)

object TargetSummaryTable extends TableHooks:
  private type Props = TargetSummaryTable

  private val ColDef = ColumnDef[TargetWithIdAndObs]

  private val IdColumnId: ColumnId           = ColumnId("id")
  private val CountColumnId: ColumnId        = ColumnId("count")
  private val ObservationsColumnId: ColumnId = ColumnId("observations")

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

  def onTextChange(e: ReactEventFromInput): Callback =
    e.target.files.toList
      .traverse(f => importTargets(dom.readReadableStream(IO(f.stream()))).compile.toList)
      .flatTap(c => IO.println(c))
      .void
      .handleErrorWith(e => IO.println(e))
      .runAsyncAndForget
    // Callback(org.scalajs.dom.window.console.log(e.target.files(0).stream()))

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // cols
      .useMemoBy((_, _) => ()) { (props, ctx) => _ =>
        def column[V](id: ColumnId, accessor: TargetWithIdAndObs => V) =
          ColDef(id, row => accessor(row), colNames(id))

        def targetUrl(targetId: Target.Id): String =
          ctx.pageUrl(AppTab.Targets, props.programId, Focused.target(targetId))

        def obsUrl(targetId: Target.Id, obsId: Observation.Id): String =
          ctx.pageUrl(AppTab.Targets, props.programId, Focused.singleObs(obsId, targetId.some))

        List(
          ColDef(
            IdColumnId,
            _.id,
            "id",
            cell =>
              <.a(
                ^.href := targetUrl(cell.value),
                ^.onClick ==> (e => e.preventDefaultCB *> props.selectTarget(cell.value)),
                cell.value.toString
              )
          ).sortable
        ) ++
          TargetColumns
            .BaseColumnBuilder(ColDef, _.target.some)
            .allColumns ++
          List(
            column(CountColumnId, _.obsIds.size) // TODO Right align
              .copy(cell = _.value.toString),
            column(ObservationsColumnId, x => (x.id, x.obsIds.toList))
              .copy(
                cell = cell =>
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
                ,
                enableSorting = false
              )
          )
      }
      // rows
      .useMemoBy((props, _, _) => props.targets.get)((_, _, _) =>
        _.toList.map((id, targetWithObs) => TargetWithIdAndObs(id, targetWithObs))
      )
      .useReactTableWithStateStoreBy((props, ctx, cols, rows) =>
        import ctx.given

        TableOptionsWithStateStore(
          TableOptions(
            cols,
            rows,
            getRowId = (row, _, _) => RowId(row.id.toString),
            enableSorting = true,
            enableColumnResizing = true,
            enableMultiRowSelection = true,
            columnResizeMode = raw.mod.ColumnResizeMode.onChange,
            initialState = TableState(
              columnVisibility = TargetColumns.DefaultVisibility,
              rowSelection = RowSelection()
            )
          ),
          TableStore(props.userId, TableId.TargetsSummary, cols)
        )
      )
      .render((props, ctx, _, _, table) =>
        import ctx.given

        val selectedRows    = table.getSelectedRowModel().rows.toList
        val selectedRowsIds = selectedRows.map(_.original.id)

        def deleteSelected: Callback =
          ConfirmDialog.confirmDialog(
            message = <.div(
              <.div(s"This action will delete ${selectedRows.length} targets."),
              <.div("This is not undoable, Are you sure?")
            ),
            header = "Targets delete",
            acceptLabel = "Yes, delete",
            position = DialogPosition.Top,
            accept = props.targets
              .mod(_.filter((id, _) => !selectedRowsIds.contains(id))) *> TargetSummaryActions
              .deleteTargets(selectedRowsIds, props.programId)
              .runAsyncAndForget,
            acceptClass = PrimeStyles.ButtonSmall,
            rejectClass = PrimeStyles.ButtonSmall,
            icon = Icons.SkullCrossBones.color("red")
          )

        <.div(
          props.renderInTitle(
            React.Fragment(
              <.div(
                ExploreStyles.TableSelectionToolbar,
                <.label(PrimeStyles.ButtonSmall,
                        PrimeStyles.ButtonIcon,
                        ^.cls     := "p-button p-fileupload",
                        ^.htmlFor := "target-import",
                        Icons.FileImport
                ),
                <.input(^.tpe     := "file",
                        ^.onChange ==> onTextChange,
                        ^.id      := "target-import",
                        ^.name    := "file",
                        ^.accept  := ".csv"
                ),
                Button(
                  size = Button.Size.Small,
                  icon = Icons.CheckDouble,
                  onClick = table.toggleAllRowsSelected(true)
                )(" All"),
                Button(
                  size = Button.Size.Small,
                  icon = Icons.SquareXMark,
                  onClick = table.toggleAllRowsSelected(false)
                )(" None"),
                Button(
                  size = Button.Size.Small,
                  icon = Icons.Trash,
                  onClick = deleteSelected
                ).when(selectedRows.nonEmpty),
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
          PrimeTable(
            table,
            striped = true,
            compact = Compact.Very,
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
            cellMod = cell => columnClasses.get(ColumnId(cell.column.id)).orEmpty
          )
        )
      )
