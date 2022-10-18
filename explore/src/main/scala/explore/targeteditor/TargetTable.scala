// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all.*
import clue.TransactionalClient
import crystal.Pot
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import crystal.react.reuse.*
import explore.Icons
import explore.common.AsterismQueries
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AladinFullScreen
import explore.model.AppContext
import explore.model.Asterism
import explore.model.ObsIdSet
import explore.model.SiderealTargetWithId
import explore.model.enums.TableId
import explore.model.reusability.*
import explore.model.reusability.given
import explore.syntax.ui.*
import explore.targets.TargetColumns
import explore.targets.TargetSummaryTable
import explore.utils.TableHooks
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.react.table.*
import lucuma.schemas.ObservationDB
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import org.scalablytyped.runtime.StringDictionary
import react.common.Css
import react.common.ReactFnProps
import react.semanticui.collections.table.*
import react.semanticui.elements.button.*
import react.semanticui.shorthand.*
import react.semanticui.sizes.*
import reactST.{tanstackTableCore => raw}

import java.time.Instant

import scalajs.js.JSConverters.*

case class TargetTable(
  userId:         Option[User.Id],
  obsIds:         ObsIdSet,
  targets:        View[Option[Asterism]],
  selectedTarget: View[Option[Target.Id]],
  vizTime:        Option[Instant],
  renderInTitle:  Tile.RenderInTitle,
  fullScreen:     AladinFullScreen
) extends ReactFnProps(TargetTable.component)

object TargetTable extends TableHooks:
  private type Props = TargetTable

  private val ColDef = ColumnDef[SiderealTargetWithId]

  private val columnNames: Map[String, String] = Map(
    "delete" -> " "
  ) ++ TargetColumns.allColNames

  private val columnClasses: Map[String, Css] = Map(
    "delete" -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryDelete),
    "type"   -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryType |+| ExploreStyles.WithDelete),
    "name"   -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryName |+| ExploreStyles.WithDelete)
  )

  private def deleteSiderealTarget(
    obsIds:   ObsIdSet,
    targetId: Target.Id
  )(using TransactionalClient[IO, ObservationDB]): IO[Unit] =
    AsterismQueries.removeTargetFromAsterisms[IO](obsIds.toList, targetId)

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // cols
      .useMemoBy((props, _) => (props.obsIds, props.targets.get)) { (props, ctx) => _ =>
        import ctx.given

        def column[V](id: String, accessor: SiderealTargetWithId => V) =
          ColDef(id, accessor, columnNames(id))

        List(
          ColDef(
            "delete",
            _.id,
            "",
            cell =>
              Button(
                size = Tiny,
                compact = true,
                clazz = ExploreStyles.DeleteButton |+| ExploreStyles.ObsDeleteButton,
                icon = Icons.Trash.fixedWidth(),
                onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) =>
                  e.preventDefaultCB >>
                    e.stopPropagationCB >>
                    props.targets.mod(_.flatMap(_.remove(cell.value))) >>
                    deleteSiderealTarget(props.obsIds, cell.value).runAsync
              ),
            size = 35,
            enableSorting = false
          )
        ) ++
          TargetColumns
            .BaseColumnBuilder(ColDef, _.target.some)
            .allColumns
      }
      // If vizTime is not set, change it to now
      .useEffectResultWithDepsBy((p, _, _) => p.vizTime) { (_, _, _) => vizTime =>
        IO(vizTime.getOrElse(Instant.now()))
      }
      // rows
      .useMemoBy((props, _, _, vizTime) => (props.targets.get, vizTime))((_, _, _, _) =>
        case (targets, Pot.Ready(vizTime)) => targets.foldMap(_.toSiderealAt(vizTime))
        case _                             => Nil
      )
      // Load preferences
      .customBy((props, ctx, cols, _, _) =>
        useTablePreferencesLoad(
          props.userId,
          ctx,
          TableId.AsterismTargets,
          cols.value,
          TargetSummaryTable.TargetSummaryHiddenColumns
        )
      )
      .useReactTableBy((props, _, cols, _, rows, prefs) =>
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
      .customBy((_, _, _, _, _, prefs, table) =>
        useTablePreferencesStore(
          prefs,
          table
        )
      )
      .render((props, _, _, _, rows, prefs, table, _) =>
        React.Fragment(
          props.renderInTitle(
            <.span(ExploreStyles.TitleSelectColumns)(
              NewColumnSelector(table, columnNames, prefs, ExploreStyles.SelectColumns)
                .unless(props.fullScreen.value)
            )
          ),
          if (rows.isEmpty) {
            <.div(
              ExploreStyles.FullHeightWidth |+| ExploreStyles.HVCenter |+| ExploreStyles.EmptyTreeContent,
              <.div("Add a target")
            )
          } else {
            <.div(ExploreStyles.ExploreTable |+| ExploreStyles.AsterismTable)(
              PrimeTable(
                table,
                striped = true,
                compact = Compact.Very,
                tableMod = ExploreStyles.ExploreTable,
                headerCellMod = headerCell =>
                  columnClasses
                    .get(headerCell.column.id)
                    .orEmpty |+| ExploreStyles.StickyHeader,
                rowMod = row =>
                  TagMod(
                    ExploreStyles.TableRowSelected
                      .when_(props.selectedTarget.get.exists(_ === row.original.id)),
                    ^.onClick --> props.selectedTarget.set(row.original.id.some)
                  ),
                cellMod = cell => columnClasses.get(cell.column.id).orEmpty
              )
            )
          }
        )
      )
