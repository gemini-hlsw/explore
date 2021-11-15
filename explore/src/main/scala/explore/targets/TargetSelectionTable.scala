package explore.targets

import cats.syntax.all._
import crystal.react.reuse._
import lucuma.core.model.Target
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common.ReactFnProps
import explore.model.reusability._
import reactST.reactTable._
import scalajs.js.JSConverters._
import react.semanticui.collections.table.Table
import react.semanticui.collections.table.TableHeaderCell
import explore.components.ui.ExploreStyles
import react.semanticui.collections.table.TableCompact
import react.semanticui.collections.table.TableCell
import react.common.style.Css
import react.common.implicits._
import react.semanticui.elements.button.Button
import react.semanticui.sizes

final case class TargetSelectionTable(
  targets:    List[Target],
  // hiddenColumns: View[Set[String]],
  onSelected: Target ==> Callback
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
    .renderWithReuse((_, _, _, tableInstance) =>
      TargetTableComponent(
        table =
          Table(celled = true, selectable = true, striped = true, compact = TableCompact.Very)(),
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
}
