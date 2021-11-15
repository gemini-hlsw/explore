package explore.targets

import cats.syntax.all._
import crystal.react.reuse._
import lucuma.core.model.Target
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common.ReactFnProps
import explore.model.reusability._
import reactST.reactTable._
import reactST.reactTable.mod.IdType
import scalajs.js.JSConverters._
import explore.View
import crystal.react.implicits._
import react.semanticui.collections.table.Table
import react.semanticui.collections.table.TableHeaderCell
import explore.components.ui.ExploreStyles
import react.semanticui.collections.table.TableCompact
import react.semanticui.collections.table.TableCell
import react.common.style.Css
import react.common.implicits._

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

  private val columnNames: Map[String, String] = Map(
    "select" -> " "
  ) ++ TargetColumns.allColNames

  private val columnClasses: Map[String, Css] = Map(
    "type" -> (ExploreStyles.Sticky |+| ExploreStyles.TargetSummaryType),
    "name" -> (ExploreStyles.Sticky |+| ExploreStyles.TargetSummaryName)
  )

  protected val component = ScalaFnComponent
    .withHooks[Props]
    // cols
    .useMemoBy(_ => ()) { props => _ =>
      // implicit val ctx = props.ctx

      def column[V](id: String, accessor: Target => V) =
        TargetTable
          .Column(id, accessor)
          .setHeader(columnNames(id))

      // List(
      //   column("delete", TargetWithId.id.get)
      //     .setCell(cell =>
      //       Button(
      //         size = Tiny,
      //         compact = true,
      //         clazz = ExploreStyles.DeleteButton |+| ExploreStyles.ObsDeleteButton,
      //         icon = Icons.Trash,
      //         onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) =>
      //           e.preventDefaultCB >>
      //             e.stopPropagationCB >>
      //             props.targets.mod(_ - cell.value) >>
      //             deleteSiderealTarget(cell.value).runAsyncAndForget
      //       )
      //     )
      //     .setWidth(30)
      //     .setDisableSortBy(true)
      // ) ++
      TargetColumns
        .BaseColumnBuilder(TargetTable)(_.some)
        .allColumns
    }
    // rows
    .useMemoBy((props, _) => props.targets)((_, _) => identity)
    // table
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
                    .map(col => col: IdType[Target])
                    .toJSArray
                )
            )
        }.reuseCurrying(Set.empty) // props.hiddenColumns.get)
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
