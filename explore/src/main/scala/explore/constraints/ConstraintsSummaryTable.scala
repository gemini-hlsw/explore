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
import explore.model.reusability.given
import explore.model.syntax.all.*
import explore.syntax.ui.*
import explore.utils.TableHooks
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.react.table.*
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import react.common.Css
import react.common.ReactFnProps
import reactST.{tanstackTableCore => raw}

import scala.collection.immutable.SortedSet

case class ConstraintsSummaryTable(
  userId:         Option[User.Id],
  programId:      Program.Id,
  constraintList: ConstraintGroupList,
  expandedIds:    View[SortedSet[ObsIdSet]],
  renderInTitle:  Tile.RenderInTitle
) extends ReactFnProps(ConstraintsSummaryTable.component)

object ConstraintsSummaryTable extends TableHooks:
  private type Props = ConstraintsSummaryTable

  private val ColDef = ColumnDef[ConstraintGroup]

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

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useMemoBy((_, _) => ())( // Cols never changes, but needs access to props
        (props, ctx) =>
          _ =>
            def column[V](id: String, accessor: ConstraintGroup => V)
              : ColumnDef.Single[ConstraintGroup, V] =
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
              column("minam",
                     ConstraintGroup.constraintSet.andThen(ConstraintSet.elevationRange).get
              )
                .copy(cell = _.value match
                  case ElevationRange.AirMass(min, _) => f"${min.value}%.1f"
                  case ElevationRange.HourAngle(_, _) => ""
                )
                .sortableBy(_ match
                  case ElevationRange.AirMass(min, _) => min.value
                  case ElevationRange.HourAngle(_, _) => ElevationRange.AirMass.MinValue - 1
                ),
              column("maxam",
                     ConstraintGroup.constraintSet.andThen(ConstraintSet.elevationRange).get
              )
                .copy(cell = _.value match
                  case ElevationRange.AirMass(_, max) => f"${max.value}%.1f"
                  case ElevationRange.HourAngle(_, _) => ""
                )
                .sortableBy(_ match
                  case ElevationRange.AirMass(_, max) => max.value
                  case ElevationRange.HourAngle(_, _) => ElevationRange.AirMass.MinValue - 1
                ),
              column("minha",
                     ConstraintGroup.constraintSet.andThen(ConstraintSet.elevationRange).get
              )
                .copy(cell = _.value match
                  case ElevationRange.AirMass(_, _)     => ""
                  case ElevationRange.HourAngle(min, _) => f"${min.value}%.1f"
                )
                .sortableBy(_ match
                  case ElevationRange.AirMass(_, _)     => ElevationRange.HourAngle.MinHour - 1
                  case ElevationRange.HourAngle(min, _) => min.value
                ),
              column("maxha",
                     ConstraintGroup.constraintSet.andThen(ConstraintSet.elevationRange).get
              )
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
      )
      .customBy((props, ctx, cols) =>
        useTablePreferencesLoad(
          props.userId,
          ctx,
          TableId.ConstraintsSummary,
          cols.value,
          List(TableColumnPref("minam"), TableColumnPref("minha"), TableColumnPref("maxha"))
        )
      )
      // Memo rows
      .useMemoBy((props, _, _, _) => props.constraintList)((_, _, _, _) => _.values.toList)
      .useReactTableBy((props, _, cols, prefs, rows) =>
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
      )
      .customBy((_, _, _, prefs, _, table) =>
        useTablePreferencesStore(
          prefs,
          table
        )
      )
      .render { (props, ctx, _, prefs, _, table, _) =>
        import ctx.given

        <.div(
          props.renderInTitle(
            React.Fragment(
              <.span, // Push column selector to right
              <.span(ExploreStyles.TitleSelectColumns)(
                NewColumnSelector(
                  table,
                  columnNames,
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
      }
