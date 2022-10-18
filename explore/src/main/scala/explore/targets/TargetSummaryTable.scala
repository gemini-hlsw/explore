// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import explore.model.enums.TableId
import cats.Order.*
import cats.syntax.all.*
import crystal.react.View
import crystal.react.reuse.*
import explore.common.AsterismQueries.*
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.TableColumnPref
import explore.model.TargetWithIdAndObs
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.react.table.*
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import org.scalablytyped.runtime.StringDictionary
import react.common.Css
import react.common.ReactFnProps
import react.semanticui.collections.table.*
import explore.utils.TableHooks
import explore.syntax.ui.*
import reactST.{tanstackTableCore => raw}

import scalajs.js.JSConverters.*
import explore.model.AppContext
import lucuma.core.model.User
import lucuma.core.enums.Band

case class TargetSummaryTable(
  userId:            Option[User.Id],
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

  val TargetSummaryHiddenColumns: List[TableColumnPref] =
    (List("epoch", "pmra", "pmdec", "z", "cz", "parallax", "morphology", "sed") ++
      Band.all
        .filterNot(_ === Band.V)
        .map(b => b.shortName + "mag")).map(TableColumnPref.apply)

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // cols
      .useMemoBy((_, _) => ()) { (props, _) => _ =>
        def column[V](id: String, accessor: TargetWithIdAndObs => V) =
          ColDef(id, row => accessor(row), TargetColumns.allColNames(id))

        List(
          ColDef(
            "id",
            _.id,
            "id",
            cell => <.a(^.onClick ==> (_ => props.selectTarget(cell.value)), cell.value.toString)
          ).sortable
        ) ++
          TargetColumns
            .BaseColumnBuilder(ColDef, _.target.some)
            .allColumns ++
          List(
            column("count", _.obsIds.size) // TODO Right align
              .copy(cell = _.value.toString),
            column("observations", _.obsIds.toList)
              .copy(
                cell = cell =>
                  <.span(
                    cell.value
                      .map(obsId =>
                        <.a(
                          ^.onClick ==> (_ => props.selectObservation(obsId, cell.row.original.id)),
                          obsId.toString
                        )
                      )
                      .mkReactFragment(", ")
                  ),
                enableSorting = false
              )
          )
      }
      // Load preferences
      .customBy((props, ctx, cols) =>
        useTablePreferencesLoad(
          props.userId,
          ctx,
          TableId.TargetsSummary,
          cols.value,
          TargetSummaryHiddenColumns
        )
      )
      // rows
      .useMemoBy((props, _, _, _) => props.targets)((_, _, _, _) =>
        _.toList.map { case (id, targetWithObs) => TargetWithIdAndObs(id, targetWithObs) }
      )
      .useReactTableBy((props, _, cols, prefs, rows) =>
        TableOptions(
          cols,
          rows,
          getRowId = (row, _, _) => row.id.toString,
          enableSorting = true,
          enableColumnResizing = true,
          columnResizeMode = raw.mod.ColumnResizeMode.onChange,
          initialState = raw.mod
            .InitialTableState()
            .setColumnVisibility(prefs.get.hiddenColumnsDictionary)
            .setSorting(toSortingRules(prefs.get.sortingColumns))
        )
      )
      .customBy((_, _, _, prefs, _, table) =>
        useTablePreferencesStore(
          prefs,
          table
        )
      )
      .render((props, _, _, prefs, _, table, _) =>
        <.div(
          props.renderInTitle(
            React.Fragment(
              <.span, // Push column selector to right
              <.span(ExploreStyles.TitleSelectColumns)(
                NewColumnSelector(
                  table,
                  TargetColumns.allColNames,
                  prefs,
                  ExploreStyles.SelectColumns
                )
              )
            )
          ),
          PrimeTable(
            table,
            striped = true,
            compact = Compact.Very,
            tableMod = ExploreStyles.ExploreTable,
            headerCellMod = headerCell =>
              columnClasses
                .get(headerCell.column.id)
                .orEmpty |+| ExploreStyles.StickyHeader,
            cellMod = cell => columnClasses.get(cell.column.id).orEmpty
          )
        )
      )
