// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.syntax.all._
import crystal.react.View
import crystal.react.implicits._
import crystal.react.reuse._
import explore.common.AsterismQueries._
import explore.components.Tile
import explore.components.ui.ExploreStyles
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
  targetGroupList:   TargetGroupList,
  hiddenColumns:     View[Set[String]],
  selectObservation: Observation.Id ==> Callback,
  renderInTitle:     Tile.RenderInTitle
) extends ReactFnProps[TargetSummaryTable](TargetSummaryTable.component)

object TargetSummaryTable {
  type Props = TargetSummaryTable

  implicit protected val propsReuse: Reusability[Props] = Reusability.derive

  protected val TargetTable =
    TableDef[TargetGroup].withSortBy

  protected val TargetTableComponent = new SUITable(TargetTable)

  private val columnClasses: Map[String, Css] = Map(
    "type" -> (ExploreStyles.Sticky |+| ExploreStyles.TargetSummaryType),
    "name" -> (ExploreStyles.Sticky |+| ExploreStyles.TargetSummaryName)
  )

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      // cols
      .useMemoBy(_ => ()) { props => _ =>
        def column[V](id: String, accessor: TargetGroup => V) =
          TargetTable
            .Column(id, row => accessor(row))
            .setHeader(TargetColumns.allColNames(id))

        List(
          // TODO: Add Id column?
          TargetColumns.BaseColumnBuilder(TargetTable)(_.target.target.some).typeColumn,
          column("name", _.target.target.name)
            .setCell(cell =>
              // TODO: Make this clickable when it is possible to edit an individual target.
              cell.value.toString
            )
            .setSortByFn(_.toString)
        ) ++
          TargetColumns
            .NonBaseSiderealColumnBuilder(TargetTable)(_.target.target.some)
            .allColumns ++
          List(
            column("count", _.observationIds.length) // TODO Right align
              .setCell(_.value.toString)
              .setSortType(DefaultSortTypes.number),
            column("observations", _.observationIds)
              .setCell(cell =>
                <.span(
                  cell.value
                    .map(obsId =>
                      <.a(
                        ^.onClick ==> (_ => props.selectObservation(obsId)),
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
      .useMemoBy((props, _) => props.targetGroupList)((_, _) => _.values.toList)
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
              Dropdown(item = true,
                       simple = true,
                       pointing = Pointing.TopRight,
                       scrolling = true,
                       text = "Columns",
                       clazz = ExploreStyles.SelectColumns
              )(
                DropdownMenu()(
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
            table = Table(celled = true,
                          selectable = true,
                          striped = true,
                          compact = TableCompact.Very,
                          unstackable = true,
                          clazz = ExploreStyles.ExploreTable
            )(),
            header = true,
            headerCell = (col: TargetTable.ColumnType) =>
              TableHeaderCell(clazz = columnClasses.get(col.id.toString).orUndefined)(
                ^.textTransform.none,
                ^.whiteSpace.nowrap
              ),
            cell = (cell: TargetTable.CellType[_]) =>
              TableCell(clazz = columnClasses.get(cell.column.id.toString).orUndefined)(
                ^.whiteSpace.nowrap
              )
          )(tableInstance)
        )
      )
}
