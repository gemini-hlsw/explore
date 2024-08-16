// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import cats.Order.*
import cats.syntax.all.*
import crystal.react.*
import explore.Icons
import explore.common.UserPreferencesQueries
import explore.common.UserPreferencesQueries.TableStore
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ConstraintGroup
import explore.model.ConstraintGroupList
import explore.model.Focused
import explore.model.ObsIdSet
import explore.model.Observation
import explore.model.enums.AppTab
import explore.model.enums.TableId
import explore.model.syntax.all.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.table.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.table.hooks.*

import scala.collection.immutable.SortedSet
import explore.model.enums.TileSizeState

case class ConstraintsSummaryTileState(
  table: Option[Table[ConstraintGroup, Nothing]] = None
)

case class ConstraintsSummaryTableBody(
  userId:         Option[User.Id],
  programId:      Program.Id,
  constraintList: ConstraintGroupList,
  expandedIds:    View[SortedSet[ObsIdSet]]
)(val state: View[ConstraintsSummaryTileState])
    extends ReactFnProps(ConstraintsSummaryTableBody.component)

object ConstraintsSummaryTableBody:
  private type Props = ConstraintsSummaryTableBody

  private val ColDef = ColumnDef[ConstraintGroup]

  private val EditColumnId: ColumnId         = ColumnId("edit")
  private val IQColumnId: ColumnId           = ColumnId("iq")
  private val CCColumnId: ColumnId           = ColumnId("cc")
  private val BGColumnId: ColumnId           = ColumnId("bg")
  private val WVColumnId: ColumnId           = ColumnId("wv")
  private val MinAMColumnId: ColumnId        = ColumnId("minam")
  private val MaxAMColumnId: ColumnId        = ColumnId("maxam")
  private val MinHAColumnId: ColumnId        = ColumnId("minha")
  private val MaxHAColumnId: ColumnId        = ColumnId("maxha")
  private val CountColumnId: ColumnId        = ColumnId("count")
  private val ObservationsColumnId: ColumnId = ColumnId("observations")

  val columnNames: Map[ColumnId, String] = Map(
    EditColumnId         -> " ",
    IQColumnId           -> "IQ",
    CCColumnId           -> "CC",
    BGColumnId           -> "BG",
    WVColumnId           -> "WV",
    MinAMColumnId        -> "Min AM",
    MaxAMColumnId        -> "Max AM",
    MinHAColumnId        -> "Min HA",
    MaxHAColumnId        -> "Max HA",
    CountColumnId        -> "Count",
    ObservationsColumnId -> "Observations"
  )

  private val columnClasses: Map[ColumnId, Css] = Map(
    EditColumnId -> (ExploreStyles.StickyColumn |+| ExploreStyles.ConstraintsSummaryEdit)
  )

  private val DefaultColVisibility: ColumnVisibility =
    ColumnVisibility(
      MinAMColumnId -> Visibility.Hidden,
      MinHAColumnId -> Visibility.Hidden,
      MaxHAColumnId -> Visibility.Hidden
    )

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useMemoBy((_, _) => ())( // Cols never changes, but needs access to props
        (props, ctx) =>
          _ =>
            def column[V](id: ColumnId, accessor: ConstraintGroup => V)
              : ColumnDef.Single.NoMeta[ConstraintGroup, V] =
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
              column(EditColumnId, ConstraintGroup.obsIds.get)
                .setCell(cell =>
                  <.a(^.href := obsSetUrl(cell.value),
                      ^.onClick ==> (_.preventDefaultCB *> goToObsSet(cell.value)),
                      Icons.Edit
                  )
                )
                .setEnableSorting(false),
              column(
                IQColumnId,
                ConstraintGroup.constraintSet.andThen(ConstraintSet.imageQuality).get
              )
                .setCell(_.value.label)
                .sortableBy(_.label),
              column(
                CCColumnId,
                ConstraintGroup.constraintSet.andThen(ConstraintSet.cloudExtinction).get
              )
                .setCell(_.value.label)
                .sortableBy(_.label),
              column(
                BGColumnId,
                ConstraintGroup.constraintSet.andThen(ConstraintSet.skyBackground).get
              )
                .setCell(_.value.label)
                .sortableBy(_.label),
              column(
                WVColumnId,
                ConstraintGroup.constraintSet.andThen(ConstraintSet.waterVapor).get
              )
                .setCell(_.value.label)
                .sortableBy(_.label),
              column(
                MinAMColumnId,
                ConstraintGroup.constraintSet.andThen(ConstraintSet.elevationRange).get
              )
                .setCell(_.value match
                  case ElevationRange.AirMass(min, _) => f"${min.value}%.1f"
                  case ElevationRange.HourAngle(_, _) => ""
                )
                .sortableBy(_ match
                  case ElevationRange.AirMass(min, _) => min.value
                  case ElevationRange.HourAngle(_, _) => ElevationRange.AirMass.MinValue - 1
                ),
              column(
                MaxAMColumnId,
                ConstraintGroup.constraintSet.andThen(ConstraintSet.elevationRange).get
              )
                .setCell(_.value match
                  case ElevationRange.AirMass(_, max) => f"${max.value}%.1f"
                  case ElevationRange.HourAngle(_, _) => ""
                )
                .sortableBy(_ match
                  case ElevationRange.AirMass(_, max) => max.value
                  case ElevationRange.HourAngle(_, _) => ElevationRange.AirMass.MinValue - 1
                ),
              column(
                MinHAColumnId,
                ConstraintGroup.constraintSet.andThen(ConstraintSet.elevationRange).get
              )
                .setCell(_.value match
                  case ElevationRange.AirMass(_, _)     => ""
                  case ElevationRange.HourAngle(min, _) => f"${min.value}%.1f"
                )
                .sortableBy(_ match
                  case ElevationRange.AirMass(_, _)     => ElevationRange.HourAngle.MinHour - 1
                  case ElevationRange.HourAngle(min, _) => min.value
                ),
              column(
                MaxHAColumnId,
                ConstraintGroup.constraintSet.andThen(ConstraintSet.elevationRange).get
              )
                .setCell(_.value match
                  case ElevationRange.AirMass(_, _)     => ""
                  case ElevationRange.HourAngle(_, max) => f"${max.value}%.1f"
                )
                .sortableBy(_ match
                  case ElevationRange.AirMass(_, _)     => ElevationRange.HourAngle.MinHour - 1
                  case ElevationRange.HourAngle(_, max) => max.value
                ),
              column(CountColumnId, _.obsIds.length),
              column(ObservationsColumnId, ConstraintGroup.obsIds.get)
                .setCell(cell =>
                  <.span(
                    cell.value.toSortedSet.toList
                      .map(obsId =>
                        <.a(
                          ^.href := obsUrl(obsId),
                          ^.onClick ==> (_.preventDefaultCB
                            >> goToObs(obsId)
                            >> props.expandedIds.mod(_ + cell.value)
                            >> goToObsSet(ObsIdSet.one(obsId))),
                          obsId.toString
                        )
                      )
                      .mkReactFragment(", ")
                  )
                )
                .setEnableSorting(false)
            )
      )
      // Memo rows
      .useMemoBy((props, _, _) => props.constraintList)((_, _, _) =>
        _.map(ConstraintGroup.fromTuple).toList.sortBy(_.constraintSet.summaryString)
      )
      .useReactTableWithStateStoreBy((props, ctx, cols, rows) =>
        import ctx.given

        TableOptionsWithStateStore(
          TableOptions(
            cols,
            rows,
            getRowId = (row, _, _) => RowId(row.constraintSet.toString),
            enableSorting = true,
            enableColumnResizing = true,
            columnResizeMode = ColumnResizeMode.OnChange,
            initialState = TableState(columnVisibility = DefaultColVisibility)
          ),
          TableStore(props.userId, TableId.ConstraintsSummary, cols)
        )
      )
      .useEffectOnMountBy((p, _, _, _, table) =>
        p.state.set(ConstraintsSummaryTileState(table.some))
      )
      .render { (props, _, _, _, table) =>
        <.div(
          PrimeTable(
            table,
            striped = true,
            compact = Compact.Very,
            tableMod = ExploreStyles.ExploreTable,
            emptyMessage = <.div("No constraints present"),
            headerCellMod = headerCell =>
              columnClasses
                .get(headerCell.column.id)
                .orEmpty |+| ExploreStyles.StickyHeader,
            cellMod = cell => columnClasses.get(cell.column.id).orEmpty
          )
        )
      }

case class ConstraintsSummaryTableTitle(
  state: View[ConstraintsSummaryTileState]
) extends ReactFnProps(ConstraintsSummaryTableTitle.component)

object ConstraintsSummaryTableTitle:
  private type Props = ConstraintsSummaryTableTitle

  private val component =
    ScalaFnComponent[Props]: props =>
      React.Fragment(
        <.span, // Push column selector to right
        <.span(ExploreStyles.TitleSelectColumns)(
          props.state.get.table.map(
            ColumnSelector(_, ConstraintsSummaryTableBody.columnNames, ExploreStyles.SelectColumns)
          )
        )
      )
