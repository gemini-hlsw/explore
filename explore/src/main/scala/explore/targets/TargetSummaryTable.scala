// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order._
import cats.syntax.all._
import crystal.react.View
import crystal.react.reuse._
import explore.common.AsterismQueries._
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.TargetWithIdAndObs
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.ui.reusability._
import react.common._
import react.common.implicits._
import react.semanticui.collections.table._
import react.semanticui.modules.checkbox.Checkbox
import react.semanticui.modules.dropdown.DropdownItem
import react.semanticui.modules.dropdown._
import reactST.reactTable._
import reactST.reactTable.mod.DefaultSortTypes
import reactST.reactTable.mod.IdType

import scalajs.js.JSConverters._

final case class TargetSummaryTable(
  targets:           TargetWithObsList,
  hiddenColumns:     View[Set[String]],
  selectObservation: (Observation.Id, Target.Id) => Callback,
  selectTarget:      Target.Id => Callback,
  renderInTitle:     Tile.RenderInTitle
) extends ReactFnProps[TargetSummaryTable](TargetSummaryTable.component)

object TargetSummaryTable {
  type Props = TargetSummaryTable

  protected val TargetTable = TableDef[TargetWithIdAndObs].withSortBy

  protected val TargetTableComponent = new SUITable(TargetTable)

  private val columnClasses: Map[String, Css] = Map(
    "id"   -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryId),
    "type" -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryType |+| ExploreStyles.WithId),
    "name" -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryName |+| ExploreStyles.WithId)
  )

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      // cols
      .useMemoBy(_ => ()) { props => _ =>
        def column[V](id: String, accessor: TargetWithIdAndObs => V) =
          TargetTable
            .Column(id, row => accessor(row))
            .setHeader(TargetColumns.allColNames(id))

        List(
          // TODO: Add a delete button
          TargetTable
            .Column("id", _.id)
            .setHeader("id")
            .setCell(cell =>
              <.a(^.onClick ==> (_ => props.selectTarget(cell.value)), cell.value.toString)
            )
            .setSortByAuto
        ) ++
          TargetColumns
            .BaseColumnBuilder(TargetTable)(_.target.some)
            .allColumns ++
          List(
            column("count", _.obsIds.size) // TODO Right align
              .setCell(_.value.toString)
              .setSortType(DefaultSortTypes.number),
            column("observations", _.obsIds.toList)
              .setCell(cell =>
                <.span(
                  cell.value
                    .map(obsId =>
                      <.a(
                        ^.onClick ==> (_ => props.selectObservation(obsId, cell.row.original.id)),
                        obsId.toString
                      )
                    )
                    .mkReactFragment(", ")
                )
              )
              .setDisableSortBy(true)
          )
      }
      // rows
      .useMemoBy((props, _) => props.targets)((_, _) =>
        _.toList.map { case (id, targetWithObs) => TargetWithIdAndObs(id, targetWithObs) }
      )
      .useTableBy((props, cols, rows) =>
        TargetTable(
          cols,
          rows,
          { (hiddenColumns: Set[String], options: TargetTable.OptionsType) =>
            options
              .setAutoResetSortBy(false)
              .setInitialState(
                TargetTable
                  .State()
                  .setHiddenColumns(
                    hiddenColumns.toList
                      .map(col => col: IdType[Target.Id])
                      .toJSArray
                  )
              )
          }.reuseCurrying(props.hiddenColumns.get)
        )
      )
      .render((props, _, _, tableInstance) =>
        <.div(
          props.renderInTitle(
            <.span(ExploreStyles.TitleSelectColumns)(
              Dropdown(
                item = true,
                simple = true,
                pointing = Pointing.TopRight,
                scrolling = true,
                text = "Columns",
                clazz = ExploreStyles.SelectColumns
              )(
                DropdownMenu(
                  tableInstance.allColumns
                    .drop(2)
                    .toTagMod { column =>
                      val colId = column.id.toString
                      DropdownItem()(^.key := colId)(
                        <.div(
                          Checkbox(
                            label = TargetColumns.allColNames(colId),
                            checked = column.isVisible,
                            onChange = (value: Boolean) =>
                              Callback(column.toggleHidden()) >>
                                props.hiddenColumns
                                  .mod(cols => if (value) cols - colId else cols + colId)
                          )
                        )
                      )
                    }
                )
              )
            )
          ),
          TargetTableComponent(
            table = Table(
              celled = true,
              selectable = true,
              striped = true,
              compact = TableCompact.Very,
              unstackable = true,
              clazz = ExploreStyles.ExploreTable
            )(),
            header = true,
            headerCell = (col: TargetTable.ColumnType) =>
              TableHeaderCell(clazz =
                columnClasses.get(col.id.toString).orEmpty |+| ExploreStyles.StickyHeader
              ),
            cell = (cell: TargetTable.CellType[?]) =>
              TableCell(clazz = columnClasses.get(cell.column.id.toString).orEmpty)
          )(tableInstance)
        )
      )
}
