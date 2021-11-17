// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.syntax.all._
import crystal.react.reuse._
import explore.components.ui.ExploreStyles
import explore.model.reusability._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Target
import react.common.ReactFnProps
import react.common.implicits._
import react.common.style.Css
import react.semanticui.collections.table.Table
import react.semanticui.collections.table.TableCell
import react.semanticui.collections.table.TableCompact
import react.semanticui.collections.table.TableHeaderCell
import react.semanticui.elements.button.Button
import react.semanticui.sizes
import reactST.reactTable._

import scalajs.js.JSConverters._
import react.semanticui.collections.table.TableRow

final case class TargetSelectionTable(
  targets:       List[Target],
  onSelected:    Target ==> Callback,
  selectedIndex: Option[Int],
  onClick:       (Target, Int) ==> Callback
) extends ReactFnProps[TargetSelectionTable](TargetSelectionTable.component)

object TargetSelectionTable {
  type Props = TargetSelectionTable

  implicit protected val propsReuse: Reusability[Props] = Reusability.derive

  protected val TargetTable = TableDef[Target].withSortBy

  protected val TargetTableComponent = new SUITable(TargetTable)

  private val columnClasses: Map[String, Css] = Map(
    "type" -> (ExploreStyles.Sticky |+| ExploreStyles.TargetSummaryType),
    "name" -> (ExploreStyles.Sticky |+| ExploreStyles.TargetSummaryName)
  )

  protected val component = ScalaFnComponent
    .withHooks[Props]
    // cols
    .useMemoBy(_ => ()) { props => _ =>
      List(
        TargetTable
          .Column("select", target => target)
          .setCell(cell =>
            Button(size = sizes.Tiny, compact = true, onClick = props.onSelected(cell.value))(
              ^.tpe := "button"
            )("Select")
          )
          .setWidth(30)
          .setDisableSortBy(true)
      ) ++
        TargetColumns
          .BaseColumnBuilder(TargetTable)(_.some)
          .allColumns
    }
    // rows
    .useMemoBy((props, _) => props.targets)((_, _) => identity)
    // table
    .useTableBy((_, cols, rows) =>
      TargetTable(
        cols,
        rows,
        Reuse.always((options: TargetTable.OptionsType) =>
          options
            .setAutoResetSortBy(false)
        )
      )
    )
    // .useMemo
    .renderWithReuse((props, _, _, tableInstance) =>
      TargetTableComponent(
        table = Table(celled = true,
                      selectable = true,
                      striped = true,
                      compact = TableCompact.Very,
                      clazz = ExploreStyles.ExploreTable
        )(),
        header = true,
        headerCell = (col: TargetTable.ColumnType) =>
          TableHeaderCell(clazz = columnClasses.get(col.id.toString).orUndefined)(
            ^.textTransform.none,
            ^.whiteSpace.nowrap
          ),
        row = (rowData: TargetTable.RowType) =>
          TableRow(
            clazz = ExploreStyles.TableRowSelected.when_(
              props.selectedIndex.contains_(rowData.index.toInt)
            )
          )(
            ^.onClick --> props.onClick((rowData.original, rowData.index.toInt)),
            props2Attrs(rowData.getRowProps())
          ),
        cell = (cell: TargetTable.CellType[_]) =>
          TableCell(clazz = columnClasses.get(cell.column.id.toString).orUndefined)(
            ^.whiteSpace.nowrap
          )
      )(tableInstance)
    )
}
