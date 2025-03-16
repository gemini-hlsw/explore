// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import explore.Icons
import explore.common.UserPreferencesQueries
import explore.common.UserPreferencesQueries.TableStore
import explore.components.ColumnSelectorInTitle
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ConstraintGroup
import explore.model.ConstraintGroupList
import explore.model.ConstraintTabTileIds
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
import lucuma.core.util.NewType
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.table.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.table.hooks.*

import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeSeqMap

object ConstraintsSummaryTile:
  def apply(
    userId:         Option[User.Id],
    programId:      Program.Id,
    constraintList: ConstraintGroupList,
    expandedIds:    View[SortedSet[ObsIdSet]],
    backButton:     VdomNode
  ) =
    Tile(
      ConstraintTabTileIds.Summary.id,
      "Constraints Summary",
      TileState.Initial,
      backButton.some,
      canMinimize = false,
      canMaximize = false
    )(
      tileState =>
        Body(userId, programId, constraintList, expandedIds, tileState.zoom(TileState.value)),
      (tileState, _) =>
        ColumnSelectorInTitle(SelectableColumnNames.toList, tileState.zoom(TileState.value))
    )

  object TileState extends NewType[ColumnVisibility]:
    val Initial: TileState = TileState(DefaultColVisibility)
  type TileState = TileState.Type

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

  private val SelectableColumnNames: TreeSeqMap[ColumnId, String] =
    TreeSeqMap(
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

  private val ColumnNames: TreeSeqMap[ColumnId, String] =
    TreeSeqMap(EditColumnId -> " ") ++ SelectableColumnNames

  private val DefaultColVisibility: ColumnVisibility =
    ColumnVisibility(
      MinAMColumnId -> Visibility.Hidden,
      MinHAColumnId -> Visibility.Hidden,
      MaxHAColumnId -> Visibility.Hidden
    )

  private case class Body(
    userId:           Option[User.Id],
    programId:        Program.Id,
    constraintList:   ConstraintGroupList,
    expandedIds:      View[SortedSet[ObsIdSet]],
    columnVisibility: View[ColumnVisibility]
  ) extends ReactFnProps(Body.component)

  private object Body:
    private val ColDef = ColumnDef[ConstraintGroup]

    private val ColumnClasses: Map[ColumnId, Css] = Map(
      EditColumnId -> (ExploreStyles.StickyColumn |+| ExploreStyles.ConstraintsSummaryEdit)
    )

    private def columns(props: Body, ctx: AppContext[IO]): List[ColDef.Type] =
      def column[V](id: ColumnId, accessor: ConstraintGroup => V): ColDef.TypeFor[V] =
        ColDef(id, accessor, ColumnNames(id))

      def goToObsSet(obsIdSet: ObsIdSet): Callback =
        ctx.pushPage(
          (AppTab.Constraints, props.programId, Focused.obsSet(obsIdSet)).some
        )

      def obsSetUrl(obsIdSet: ObsIdSet): String =
        ctx.pageUrl(
          (AppTab.Constraints, props.programId, Focused.obsSet(obsIdSet)).some
        )

      def goToObs(obsId: Observation.Id): Callback =
        ctx.pushPage(
          (AppTab.Constraints, props.programId, Focused.singleObs(obsId)).some
        )

      def obsUrl(obsId: Observation.Id): String =
        ctx.pageUrl(
          (AppTab.Constraints, props.programId, Focused.singleObs(obsId)).some
        )

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
            case ElevationRange.HourAngle(_, _) =>
              ElevationRange.AirMass.MinValue - 1
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
            case ElevationRange.HourAngle(_, _) =>
              ElevationRange.AirMass.MinValue - 1
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
            case ElevationRange.AirMass(_, _)     =>
              ElevationRange.HourAngle.MinHour - 1
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
            case ElevationRange.AirMass(_, _)     =>
              ElevationRange.HourAngle.MinHour - 1
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

    private val component =
      ScalaFnComponent[Body]: props =>
        for {
          ctx   <- useContext(AppContext.ctx)
          cols  <- useMemo(()): // Cols never changes, but needs access to props
                     _ => columns(props, ctx)
          // Memo rows
          rows  <- useMemo(props.constraintList):
                     _.map(ConstraintGroup.fromTuple).toList.sortBy(_.constraintSet.summaryString)
          table <- useReactTableWithStateStore:
                     import ctx.given

                     TableOptionsWithStateStore(
                       TableOptions(
                         cols,
                         rows,
                         getRowId = (row, _, _) => RowId(row.constraintSet.toString),
                         enableSorting = true,
                         enableColumnResizing = true,
                         columnResizeMode = ColumnResizeMode.OnChange,
                         state = PartialTableState(columnVisibility = props.columnVisibility.get),
                         onColumnVisibilityChange = stateInViewHandler(props.columnVisibility.mod)
                       ),
                       TableStore(props.userId, TableId.ConstraintsSummary, cols)
                     )
        } yield <.div(
          PrimeTable(
            table,
            striped = true,
            compact = Compact.Very,
            tableMod = ExploreStyles.ExploreTable,
            emptyMessage = <.div("No constraints present"),
            headerCellMod = headerCell =>
              ColumnClasses
                .get(headerCell.column.id)
                .orEmpty |+| ExploreStyles.StickyHeader,
            cellMod = cell => ColumnClasses.get(cell.column.id).orEmpty
          )
        )
