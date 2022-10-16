// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import cats.Functor
import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import crystal.react.reuse.*
import explore.Icons
import explore.common.ConstraintGroupQueries.*
import explore.common.UserPreferencesQueries.TableColumns
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ColumnId
import explore.model.ConstraintGroup
import explore.model.Focused
import explore.model.ObsIdSet
import explore.model.TableColumnPref
import explore.model.enums.AppTab
import explore.model.enums.TableId
import explore.model.syntax.all.*
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.react.table.*
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import monocle.function.Each.each
import monocle.function.Each.listEach
import monocle.std.list.*
import org.scalablytyped.runtime.StringDictionary
import react.common.Css
import react.common.ReactFnProps
import react.semanticui.collections.table.*
import reactST.{tanstackTableCore => raw}

import scala.collection.immutable.SortedSet

import scalajs.js.JSConverters.*
import scalajs.js
import lucuma.core.model.Observation
import explore.model.enums.SortDirection

case class ConstraintsSummaryTable(
  userId:         Option[User.Id],
  programId:      Program.Id,
  constraintList: ConstraintGroupList,
  expandedIds:    View[SortedSet[ObsIdSet]],
  renderInTitle:  Tile.RenderInTitle
) extends ReactFnProps(ConstraintsSummaryTable.component)

object ConstraintsSummaryTable:
  private type Props = ConstraintsSummaryTable

  private val ColDef = ColumnDef[ConstraintGroup]

  given Reusability[TableColumnPref] = Reusability.byEq

  private val columnNames: Map[String, String] = Map(
    "edit"         -> " ",
    "iq"           -> "IQ",
    "cc"           -> "CC",
    "bg"           -> "BG",
    "wv"           -> "WV",
    "minam"        -> "Min AM",
    "maxam"        -> "Max AM",
    "minha"        -> "Min HA",
    "maxha"        -> "Max HA",
    "count"        -> "Count",
    "observations" -> "Observations"
  )

  private val columnClasses: Map[String, Css] = Map(
    "edit" -> (ExploreStyles.StickyColumn |+| ExploreStyles.ConstraintsSummaryEdit)
  )

  private def toSortingRules(tuples: List[(String, Boolean)]): js.Array[raw.mod.ColumnSort] =
    // println(s"TO sorting $tuples")
    tuples.map { case (id, b) => raw.mod.ColumnSort(b, id) }.toJSArray

  private def fromTableState(state: raw.mod.TableState): List[(String, Boolean)] =
    // println(s"From sorting ${state.sorting.toList.map(sr => (sr.id.toString, sr.desc))}")
    state.sorting.toList.map(sr => (sr.id.toString, sr.desc))

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateViewWithReuse(
        List(TableColumnPref("minam"), TableColumnPref("minha"), TableColumnPref("maxha"))
      )
      .useMemoBy((_, _, _) => ()) { // Cols never changes, but needs access to props
        (props, ctx, _) => _ =>
          def column[V](id: String, accessor: ConstraintGroup => V) =
            ColDef(id, accessor, columnNames(id))

          def goToObsSet(obsIdSet: ObsIdSet): Callback =
            ctx.pushPage(AppTab.Constraints, props.programId, Focused.obsSet(obsIdSet))

          def obsSetUrl(obsIdSet: ObsIdSet): String =
            ctx.pageUrl(AppTab.Constraints, props.programId, Focused.obsSet(obsIdSet))

          def goToObs(obsId: Observation.Id): Callback =
            ctx.pushPage(AppTab.Constraints, props.programId, Focused.singleObs(obsId))

          def obsUrl(obsId: Observation.Id): String =
            ctx.pageUrl(AppTab.Constraints, props.programId, Focused.singleObs(obsId))

          List(
            column("edit", ConstraintGroup.obsIds.get)
              .copy(
                cell = cell =>
                  <.a(^.href := obsSetUrl(cell.value),
                      ^.onClick ==> (_ => goToObsSet(cell.value)),
                      Icons.Edit
                  ),
                enableSorting = false
              ),
            column("iq", ConstraintGroup.constraintSet.andThen(ConstraintSet.imageQuality).get)
              .copy(cell = _.value.label)
              .sortableBy(_.label),
            column("cc", ConstraintGroup.constraintSet.andThen(ConstraintSet.cloudExtinction).get)
              .copy(cell = _.value.label)
              .sortableBy(_.label),
            column("bg", ConstraintGroup.constraintSet.andThen(ConstraintSet.skyBackground).get)
              .copy(cell = _.value.label)
              .sortableBy(_.label),
            column("wv", ConstraintGroup.constraintSet.andThen(ConstraintSet.waterVapor).get)
              .copy(cell = _.value.label)
              .sortableBy(_.label),
            column("minam", ConstraintGroup.constraintSet.andThen(ConstraintSet.elevationRange).get)
              .copy(cell = _.value match
                case ElevationRange.AirMass(min, _) => f"${min.value}%.1f"
                case ElevationRange.HourAngle(_, _) => ""
              )
              .sortableBy(_ match
                case ElevationRange.AirMass(min, _) => min.value
                case ElevationRange.HourAngle(_, _) => ElevationRange.AirMass.MinValue - 1
              ),
            column("maxam", ConstraintGroup.constraintSet.andThen(ConstraintSet.elevationRange).get)
              .copy(cell = _.value match
                case ElevationRange.AirMass(_, max) => f"${max.value}%.1f"
                case ElevationRange.HourAngle(_, _) => ""
              )
              .sortableBy(_ match
                case ElevationRange.AirMass(_, max) => max.value
                case ElevationRange.HourAngle(_, _) => ElevationRange.AirMass.MinValue - 1
              ),
            column("minha", ConstraintGroup.constraintSet.andThen(ConstraintSet.elevationRange).get)
              .copy(cell = _.value match
                case ElevationRange.AirMass(_, _)     => ""
                case ElevationRange.HourAngle(min, _) => f"${min.value}%.1f"
              )
              .sortableBy(_ match
                case ElevationRange.AirMass(_, _)     => ElevationRange.HourAngle.MinHour - 1
                case ElevationRange.HourAngle(min, _) => min.value
              ),
            column("maxha", ConstraintGroup.constraintSet.andThen(ConstraintSet.elevationRange).get)
              .copy(cell = _.value match
                case ElevationRange.AirMass(_, _)     => ""
                case ElevationRange.HourAngle(_, max) => f"${max.value}%.1f"
              )
              .sortableBy(_ match
                case ElevationRange.AirMass(_, _)     => ElevationRange.HourAngle.MinHour - 1
                case ElevationRange.HourAngle(_, max) => max.value
              ),
            column("count", _.obsIds.length),
            column("observations", ConstraintGroup.obsIds.get)
              .copy(
                cell = cell =>
                  <.span(
                    cell.value.toSortedSet.toList
                      .map(obsId =>
                        <.a(
                          ^.href := obsUrl(obsId),
                          ^.onClick ==> (_ =>
                            goToObs(obsId)
                              >> props.expandedIds.mod(_ + cell.value)
                              >> goToObsSet(ObsIdSet.one(obsId))
                          ),
                          obsId.toString
                        )
                      )
                      .mkReactFragment(", ")
                  ),
                enableSorting = false
              )
          )
      }
      .useEffectWithDepsBy((props, _, _, cols) => props.userId) { (props, ctx, prefs, cols) => _ =>
        import ctx.given
        val allColumns =
          cols.value
            .map(col => TableColumnPref(ColumnId(col.id), visible = true, None))
            .withStored(prefs.get)

        TableColumns
          .queryColumns[IO](props.userId, TableId.ConstraintsSummary)
          .flatMap(r =>
            prefs
              .set(allColumns.withStored(r.lucumaTableColumnPreferences).sortBy(_.columnId.value))
              .to[IO]
          )
          .runAsyncAndForget
      }
      // Memo rows
      .useMemoBy((props, _, _, _) => props.constraintList)((_, _, _, _) => _.values.toList)
      .useReactTableBy { (props, _, prefs, cols, rows) =>
        TableOptions(
          cols,
          rows,
          getRowId = (row, _, _) => row.constraintSet.toString,
          enableSorting = true,
          enableColumnResizing = true,
          columnResizeMode = raw.mod.ColumnResizeMode.onChange,
          initialState = raw.mod
            .InitialTableState()
            .setColumnVisibility(prefs.get.hiddenColumnsDictionary)
            .setSorting(toSortingRules(prefs.get.sortingColumns))
        )
      }
      .useEffectWithDepsBy((_, _, prefs, _, _, table) => prefs) { (_, _, prefs, _, _, table) => p =>
        Callback {
          table.setColumnVisibility(prefs.get.hiddenColumnsDictionary)
          table.setSorting(toSortingRules(prefs.get.sortingColumns))
        }
      }
      .useEffectWithDepsBy((_, _, prefs, _, _, table) => fromTableState(table.getState()))(
        (props, ctx, prefs, _, _, table) =>
          rules =>
            import ctx.given

            val prefsView = prefs.withOnMod { l =>
              TableColumns
                .storeColumns[IO](props.userId, TableId.ConstraintsSummary, l)
                .runAsyncAndForget
            }
            prefsView
              .mod(_.map {
                case t @ TableColumnPref(cid, _, _) if rules.find(_._1 === cid.value).isDefined =>
                  t.copy(sorting =
                    rules.find(_._1 === cid.value).map(r => SortDirection.fromBoolean(r._2))
                  )
                case t                                                                          =>
                  t.copy(sorting = none)
              })
      )
      .render { (props, ctx, prefs, _, _, table) =>
        import ctx.given

        val prefsView = prefs.withOnMod { l =>
          TableColumns
            .storeColumns[IO](props.userId, TableId.ConstraintsSummary, l)
            .runAsyncAndForget
        }
        <.div(
          props.renderInTitle(
            React.Fragment(
              <.span, // Push column selector to right
              <.span(ExploreStyles.TitleSelectColumns)(
                NewColumnSelector(
                  table,
                  columnNames,
                  prefsView,
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
      }
