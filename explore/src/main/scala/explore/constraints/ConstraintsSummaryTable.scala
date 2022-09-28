// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import cats.Order.*
import cats.syntax.all.*
import crystal.react.View
import crystal.react.reuse.*
import explore.Icons
import explore.common.ConstraintGroupQueries.*
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ConstraintGroup
import explore.model.Focused
import explore.model.ObsIdSet
import explore.model.enums.AppTab
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.Program
import lucuma.react.table.*
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import org.scalablytyped.runtime.StringDictionary
import react.common.Css
import react.common.ReactFnProps
import react.semanticui.collections.table.*
import reactST.{tanstackTableCore => raw}

import scala.collection.immutable.SortedSet

import scalajs.js.JSConverters.*
import scalajs.js

case class ConstraintsSummaryTable(
  programId:      Program.Id,
  constraintList: ConstraintGroupList,
  hiddenColumns:  View[Set[String]],
  summarySorting: View[List[(String, Boolean)]],
  expandedIds:    View[SortedSet[ObsIdSet]],
  renderInTitle:  Tile.RenderInTitle
) extends ReactFnProps(ConstraintsSummaryTable.component)

object ConstraintsSummaryTable:
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

  private def toSortingRules(tuples: List[(String, Boolean)]): js.Array[raw.mod.ColumnSort] =
    tuples.map { case (id, b) => raw.mod.ColumnSort(b, id) }.toJSArray

  private def fromTableState(state: raw.mod.TableState): List[(String, Boolean)] =
    state.sorting.toList.map(sr => (sr.id.toString, sr.desc))

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useMemoBy((_, _) => ()) { // Cols never changes, but needs access to props
        (props, ctx) => _ =>
          def column[V](id: String, accessor: ConstraintGroup => V) =
            ColDef(id, accessor, columnNames(id))

          def setObsSet(obsIdSet: ObsIdSet): Callback =
            ctx.pushPage(AppTab.Constraints, props.programId, Focused.obsSet(obsIdSet))

          List(
            column("edit", ConstraintGroup.obsIds.get)
              .copy(
                cell = cell => <.a(^.onClick ==> (_ => setObsSet(cell.value)), Icons.Edit),
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
                          ^.onClick ==> (_ =>
                            (ctx.pushPage(
                              AppTab.Constraints,
                              props.programId,
                              Focused.singleObs(obsId)
                            )
                              >> props.expandedIds.mod(_ + cell.value)
                              >> setObsSet(ObsIdSet.one(obsId)))
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
      .useMemoBy((props, _, _) => props.constraintList)((_, _, _) => _.values.toList) // Memo rows
      .useReactTableBy((props, _, cols, rows) =>
        TableOptions(
          cols,
          rows,
          getRowId = (row, _, _) => row.constraintSet.toString,
          enableSorting = true,
          enableColumnResizing = true,
          columnResizeMode = raw.mod.ColumnResizeMode.onChange,
          initialState = raw.mod
            .InitialTableState()
            .setColumnVisibility(
              StringDictionary(
                props.hiddenColumns.get.toList.map(col => col -> false): _*
              )
            )
            .setSorting(toSortingRules(props.summarySorting.get))
        )
      )
      .useEffectWithDepsBy((_, _, _, _, table) => fromTableState(table.getState()))(
        (props, _, _, _, _) => rules => props.summarySorting.set(rules)
      )
      .render((props, _, _, _, table) =>
        <.div(
          props.renderInTitle(
            React.Fragment(
              <.span, // Push column selector to right
              <.span(ExploreStyles.TitleSelectColumns)(
                ColumnSelector(table, columnNames, props.hiddenColumns, ExploreStyles.SelectColumns)
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
