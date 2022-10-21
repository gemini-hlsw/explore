// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.syntax.all.*
import crystal.react.reuse.*
import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.table.*
import lucuma.ui.table.*
import react.common.*
import react.semanticui.elements.button.Button
import react.semanticui.sizes

case class TargetSelectionTable(
  targets:       List[TargetSearchResult],
  onSelected:    TargetSearchResult => Callback,
  selectedIndex: Option[Int],
  onClick:       (TargetSearchResult, Int) => Callback
) extends ReactFnProps(TargetSelectionTable.component)

object TargetSelectionTable:
  private type Props = TargetSelectionTable

  private val ColDef = ColumnDef[TargetSearchResult]

  private val SelectColumnId: ColumnId = ColumnId("select")

  private val columnClasses: Map[ColumnId, Css] = Map(
    SelectColumnId             -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummarySelect),
    TargetColumns.TypeColumnId -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryType |+| ExploreStyles.WithSelect),
    TargetColumns.NameColumnId -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryName |+| ExploreStyles.WithSelect)
  )

  private val component = ScalaFnComponent
    .withHooks[Props]
    // cols
    .useMemoBy(_ => ()) { props => _ =>
      List(
        ColDef(
          SelectColumnId,
          target => target,
          "",
          cell =>
            Button(
              size = sizes.Tiny,
              compact = true,
              positive = true,
              icon = true,
              onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) =>
                e.stopPropagationCB >> props.onSelected(cell.value)
            )(
              ^.tpe := "button"
            )(
              cell.value.targetWithOptId.optId.fold(React.Fragment(Icons.New, "Add"))(_ =>
                React.Fragment(Icons.Link, "Link")
              )
            ),
          enableSorting = false
        )
      ) ++
        TargetColumns
          .BaseColumnBuilder(ColDef, _.target.some)
          .allColumns
    }
    // rows
    .useMemoBy((props, _) => props.targets)((_, _) => identity)
    // table
    .useReactTableBy((_, cols, rows) => TableOptions(cols, rows, enableSorting = true))
    // .useMemo
    .render((props, _, _, table) =>
      PrimeTable(
        table,
        striped = true,
        compact = Compact.Very,
        tableMod = ExploreStyles.ExploreTable,
        headerCellMod = headerCell =>
          columnClasses.get(ColumnId(headerCell.column.id)).orEmpty |+| ExploreStyles.StickyHeader,
        rowMod = row =>
          TagMod(
            ExploreStyles.TableRowSelected.when_(props.selectedIndex.contains_(row.index.toInt)),
            ^.onClick --> props.onClick(row.original, row.index.toInt)
          ),
        cellMod = cell => columnClasses.get(ColumnId(cell.column.id)).orEmpty
      )
    )
