// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order.*
import cats.syntax.all.*
import crystal.react.View
import crystal.react.reuse.*
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
import react.semanticui.collections.table.*
import reactST.react.reactStrings.I
import reactST.{tanstackTableCore => raw}

import scalajs.js.JSConverters.*
import react.primereact.Button
import explore.Icons
import react.primereact.PrimeStyles

case class TargetSummaryTable(
  userId:            Option[User.Id],
  programId:         Program.Id,
  targets:           TargetWithObsList,
  selectObservation: (Observation.Id, Target.Id) => Callback,
  selectTarget:      Target.Id => Callback,
  renderInTitle:     Tile.RenderInTitle
) extends ReactFnProps(TargetSummaryTable.component)

object TargetSummaryTable extends TableHooks:
  private type Props = TargetSummaryTable

  private val ColDef = ColumnDef[TargetWithIdAndObs]

  private val columnClasses: Map[String, Css] = Map(
    "id"   -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryId),
    "type" -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryType |+| ExploreStyles.WithId),
    "name" -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryName |+| ExploreStyles.WithId)
  )

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // cols
      .useMemoBy((_, _) => ()) { (props, ctx) => _ =>
        def column[V](id: String, accessor: TargetWithIdAndObs => V) =
          ColDef(id, row => accessor(row), TargetColumns.allColNames(id))

        def targetUrl(targetId: Target.Id): String =
          ctx.pageUrl(AppTab.Targets, props.programId, Focused.target(targetId))

        def obsUrl(targetId: Target.Id, obsId: Observation.Id): String =
          ctx.pageUrl(AppTab.Targets, props.programId, Focused.singleObs(obsId, targetId.some))

        List(
          ColDef(
            "id",
            _.id,
            "id",
            cell =>
              <.a(^.href := targetUrl(cell.value),
                  ^.onClick ==> (e => e.preventDefaultCB *> props.selectTarget(cell.value)),
                  cell.value.toString
              )
          ).sortable
        ) ++
          TargetColumns
            .BaseColumnBuilder(ColDef, _.target.some)
            .allColumns ++
          List(
            column("count", _.obsIds.size) // TODO Right align
              .copy(cell = _.value.toString),
            column("observations", x => (x.id, x.obsIds.toList))
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
      .useMemoBy((props, _, _) => props.targets)((_, _, _) =>
        _.toList.map { case (id, targetWithObs) => TargetWithIdAndObs(id, targetWithObs) }
      )
      .useReactTableWithStateStoreBy((props, ctx, cols, rows) =>
        import ctx.given

        TableOptionsWithStateStore(
          TableOptions(
            cols,
            rows,
            getRowId = (row, _, _) => row.id.toString,
            enableSorting = true,
            enableColumnResizing = true,
            enableMultiRowSelection = true,
            columnResizeMode = raw.mod.ColumnResizeMode.onChange,
            initialState = raw.mod
              .InitialTableState()
              .setColumnVisibility(TargetColumns.DefaultVisibility)
              .setRowSelection(StringDictionary())
          ),
          TableStore(props.userId, TableId.TargetsSummary, cols)
        )
      )
      .render((props, _, _, _, table) =>
        <.div(
          props.renderInTitle(
            React.Fragment(
              <.div(
                <.label("Select"),
                ExploreStyles.TableSelectionToolbar,
                Button(size = Button.Size.Small,
                       icon = Icons.CheckDouble,
                       onClick = Callback(table.toggleAllRowsSelected(true))
                ),
                <.label("All"),
                Button(size = Button.Size.Small,
                       icon = Icons.SquareXMark,
                       onClick = Callback(table.toggleAllRowsSelected(false))
                ),
                <.label("None")
              ),
              <.span(ExploreStyles.TitleSelectColumns)(
                NewColumnSelector(
                  table,
                  TargetColumns.allColNames,
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
                .get(headerCell.column.id)
                .orEmpty |+| ExploreStyles.StickyHeader,
            rowMod = row =>
              TagMod(
                ExploreStyles.TableRowSelected.when_(row.getIsSelected()),
                ^.onClick --> Callback.log(s"cmd ${isHotkeyPressed("cmd")}") *> Callback {
                  // If cmd is pressed add to the selection
                  if (!isCmdCtrlPressed) table.toggleAllRowsSelected(false)
                  if (isShiftPressed) {
                    // If shift is pressed extend
                    val selectedRows = table.getSelectedRowModel().rows.toList.sortBy(_.id)
                    val allRows      = table.getRowModel().rows.toList.sortBy(_.id)
                    if (selectedRows.isEmpty) row.toggleSelected()
                    else {
                      val currentId      = row.id
                      // selectedRow is not empty, these won't fail
                      val first          = selectedRows.head.id
                      val last           = selectedRows.last.id
                      val indexOfCurrent = allRows.indexWhere(_.id === currentId)
                      val indexOfFirst   = allRows.indexWhere(_.id === first)
                      val indexOfLast    = allRows.indexWhere(_.id === last)
                      if (indexOfCurrent === -1 || indexOfFirst === -1 || indexOfLast === -1) {
                        println("Illegal selection state")
                      } else {

                        if (currentId < first) {
                          table.setRowSelection(
                            StringDictionary(
                              (first -> true) :: allRows
                                .slice(indexOfCurrent, indexOfFirst)
                                .map(_.id -> true): _*
                            )
                          )
                        } else {
                          table.setRowSelection(
                            StringDictionary(
                              ((currentId -> true) :: allRows
                                .slice(indexOfLast, indexOfCurrent)
                                .map(_.id -> true)): _*
                            )
                          )
                        }
                      }
                    }
                  } else row.toggleSelected()
                }
              ),
            cellMod = cell => columnClasses.get(cell.column.id).orEmpty
          )
        )
      )
