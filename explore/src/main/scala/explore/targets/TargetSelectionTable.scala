// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.syntax.all.*
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.CatalogName
import lucuma.react.common.*
import lucuma.react.fa.FontAwesomeIcon
import lucuma.react.primereact.Button
import lucuma.react.table.*
import lucuma.ui.primereact.*
import lucuma.ui.table.*

case class TargetSelectionTable(
  source:              TargetSource[?],
  targets:             List[TargetSearchResult],
  selectExistingLabel: String,
  selectExistingIcon:  FontAwesomeIcon,
  selectNewLabel:      String,
  selectNewIcon:       FontAwesomeIcon,
  onSelected:          TargetSearchResult => Callback,
  selectedIndex:       Option[Int],
  onClick:             (TargetSearchResult, Int) => Callback
) extends ReactFnProps(TargetSelectionTable.component)

object TargetSelectionTable:
  private type Props = TargetSelectionTable

  private val ColDef = ColumnDef[TargetSearchResult]

  private val SelectColumnId: ColumnId = ColumnId("select")

  private val columnClasses: Map[ColumnId, Css] = Map(
    SelectColumnId             -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummarySelect),
    TargetColumns.TypeColumnId -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryType),
    TargetColumns.NameColumnId -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryName)
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
            val (label, icon) =
              cell.value.targetWithOptId.optId.fold(
                (props.selectNewLabel, props.selectNewIcon)
              )(_ => (props.selectExistingLabel, props.selectExistingIcon))

            Button(
              severity = Button.Severity.Success,
              label = label,
              icon = icon,
              onClickE = e => e.stopPropagationCB >> props.onSelected(cell.value)
            ).tiny.compact
          ,
          enableSorting = false
        )
      ) ++ (
        props.source match
          case TargetSource.FromCatalog(CatalogName.Simbad) =>
            TargetColumns.Builder.ForSimbad(ColDef, _.target.some).AllColumns
          case _                                            =>
            TargetColumns.Builder.ForProgram(ColDef, _.target.some).AllColumns
      )
    }
    // rows
    .useMemoBy((props, _) => props.targets)((_, _) => identity)
    // table
    .useReactTableBy((_, cols, rows) => TableOptions(cols, rows, enableSorting = true))
    .render((props, _, _, table) =>
      PrimeTable(
        table,
        striped = true,
        compact = Compact.Very,
        tableMod = ExploreStyles.ExploreTable,
        headerCellMod = headerCell =>
          columnClasses.get(headerCell.column.id).orEmpty |+| ExploreStyles.StickyHeader,
        rowMod = row =>
          TagMod(
            ExploreStyles.TableRowSelected.when_(props.selectedIndex.contains_(row.index.toInt)),
            ^.onClick --> props.onClick(row.original, row.index.toInt)
          ),
        cellMod = cell => columnClasses.get(cell.column.id).orEmpty
      )
    )
