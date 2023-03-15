// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.syntax.all.*
import crystal.react.reuse.*
import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.table.*
import lucuma.ui.primereact.*
import lucuma.ui.table.*
import react.common.*
import react.fa.FontAwesomeIcon
import react.primereact.Button
import lucuma.core.enums.CatalogName

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

  private val ColumnClasses: Map[ColumnId, TargetSource[?] => Css] = Map(
    SelectColumnId             -> (_ => ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummarySelect),
    TargetColumns.TypeColumnId -> (_ =>
      ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryType |+| ExploreStyles.WithSelect
    ),
    TargetColumns.NameColumnId -> (source =>
      (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryName) |+| (source match
        case TargetSource.FromCatalog(CatalogName.Simbad) => ExploreStyles.WithSelect
        case _                                            => ExploreStyles.WithSelectAndType
      )
    )
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
          ColumnClasses
            .get(ColumnId(headerCell.column.id))
            .map(_(props.source))
            .orEmpty |+| ExploreStyles.StickyHeader,
        rowMod = row =>
          TagMod(
            ExploreStyles.TableRowSelected.when_(props.selectedIndex.contains_(row.index.toInt)),
            ^.onClick --> props.onClick(row.original, row.index.toInt)
          ),
        cellMod = cell => ColumnClasses.get(ColumnId(cell.column.id)).map(_(props.source)).orEmpty
      )
    )
