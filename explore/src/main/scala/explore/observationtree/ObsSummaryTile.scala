// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.syntax.pot.given
import explore.Icons
import explore.common.UserPreferencesQueries
import explore.common.UserPreferencesQueries.TableStore
import explore.components.ColumnSelectorInTitle
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Asterism
import explore.model.Execution
import explore.model.Group
import explore.model.GroupList
import explore.model.ObsSummaryTabTileIds
import explore.model.Observation
import explore.model.ObservationExecutionMap
import explore.model.ObservationList
import explore.model.TargetList
import explore.model.enums.TableId
import explore.model.reusability.given
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.model.TargetWithId
import lucuma.ui.LucumaStyles
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.table.*
import lucuma.ui.table.*
import lucuma.ui.table.hooks.*
import monocle.Focus
import monocle.Iso
import monocle.Lens

import java.util.UUID

object ObsSummaryTile extends ObsSummaryColumns:
  def apply(
    userId:          Option[User.Id],
    programId:       Program.Id,
    observations:    UndoSetter[ObservationList],
    selectedObsIds:  View[List[Observation.Id]],
    groups:          View[GroupList],
    obsExecutions:   ObservationExecutionMap,
    allTargets:      TargetList,
    showScienceBand: Boolean,
    backButton:      VdomNode
  ): Tile[TileState] =
    Tile(
      ObsSummaryTabTileIds.SummaryId.id,
      s"Observations Summary (${observations.get.values.toList.filterNot(_.isCalibration).length})",
      TileState.Initial,
      backButton.some,
      canMinimize = false,
      canMaximize = false
    )(
      s =>
        Body(
          userId,
          programId,
          observations,
          selectedObsIds,
          groups,
          obsExecutions,
          allTargets,
          showScienceBand,
          s.zoom(TileState.columnVisibility),
          cb => s.zoom(TileState.toggleAllRowsSelected).set(cb.some)
        ),
      (s, _) =>
        Title(
          s.zoom(TileState.columnVisibility),
          s.get.toggleAllRowsSelected
        )
    )

  private case class Body(
    userId:                   Option[User.Id],
    programId:                Program.Id,
    observations:             UndoSetter[ObservationList],
    selectedObsIds:           View[List[Observation.Id]],
    groups:                   View[GroupList],
    obsExecutions:            ObservationExecutionMap,
    allTargets:               TargetList,
    showScienceBand:          Boolean,
    columnVisibility:         View[ColumnVisibility],
    setToggleAllRowsSelected: (Boolean => Callback) => Callback
  ) extends ReactFnProps(Body.component)

  private object Body:
    import ObsSummaryRow.*

    given Reusability[UUID]                    = Reusability.byEq
    given Reusability[ObservationExecutionMap] = Reusability.by(_.value.toList)

    private val component = ScalaFnComponent[Body]: props =>
      for {
        ctx     <- useContext(AppContext.ctx)
        cols    <- useMemo(()):                           // Columns
                     _ => columns(props.programId, ctx)
        rows    <- useMemo(
                     (props.observations.get.values.toList,
                      props.allTargets,
                      props.groups.get,
                      props.obsExecutions
                     )
                   ): (obsList, allTargets, groups, obsExecutions) =>
                     obsList
                       .filterNot(_.isCalibration)
                       .map: obs =>
                         obs -> obs.scienceTargetIds.toList
                           .map(id => allTargets.get(id).map(t => TargetWithId(id, t)))
                           .flattenOption
                       .map: (obs, targets) =>
                         val asterism = Asterism.fromTargets(targets)
                         Expandable(
                           ObsRow(
                             obs,
                             targets.headOption,
                             asterism,
                             obs.groupId.flatMap(groups.get),
                             obsExecutions.getPot(obs.id)
                           ),
                           // Only expand if there are multiple targets
                           if (targets.sizeIs > 1)
                             targets.map: target =>
                               Expandable(ExpandedTargetRow(obs, target, obs.observationTime))
                           else Nil
                         )
        table   <- useReactTableWithStateStore:
                     import ctx.given

                     val obsIds2RowSelection: Iso[List[Observation.Id], RowSelection] =
                       Iso[List[Observation.Id], RowSelection](obsIds =>
                         RowSelection:
                           obsIds.map(obsId => RowId(obsId.toString) -> true).toMap
                       )(selection =>
                         selection.value
                           .filter(_._2)
                           .keys
                           .toList
                           .map(rowId => Observation.Id.parse(rowId.value))
                           .flattenOption
                       )

                     val rowSelection: View[RowSelection] =
                       props.selectedObsIds.as(obsIds2RowSelection)

                     TableOptionsWithStateStore(
                       TableOptions(
                         cols,
                         rows,
                         enableExpanding = true,
                         getSubRows = (row, _) => row.subRows,
                         getRowId = (row, _, _) =>
                           RowId:
                             row.value.fold(
                               o => o.obs.id.toString + o.targetWithId.id.toString,
                               _.obs.id.toString
                             )
                         ,
                         enableSorting = true,
                         enableMultiRowSelection = true,
                         state = PartialTableState(
                           rowSelection = rowSelection.get,
                           columnVisibility = props.columnVisibility.get
                         ),
                         onRowSelectionChange = stateInViewHandler(rowSelection.mod),
                         onColumnVisibilityChange = stateInViewHandler(props.columnVisibility.mod)
                       ),
                       TableStore(
                         props.userId,
                         TableId.ObservationsSummary,
                         cols,
                         ColumnsExcludedFromVisibility
                       )
                     )
        _       <- useEffectOnMount:
                     props.setToggleAllRowsSelected(table.toggleAllRowsSelected)
        _       <- useEffectWithDeps(props.showScienceBand): showScienceBand =>
                     table
                       .getColumn(ScienceBandColumnId.value)
                       .foldMap(_.toggleVisibility(showScienceBand))
        resizer <- useResizeDetector
        adding  <- useStateView(AddingObservation(false)) // adding new observation
      } yield PrimeAutoHeightVirtualizedTable(
        table,
        _ => 32.toPx,
        striped = true,
        compact = Compact.Very,
        innerContainerMod = ^.width := "100%",
        containerRef = resizer.ref,
        hoverableRows = rows.nonEmpty,
        tableMod =
          ExploreStyles.ExploreTable |+| ExploreStyles.ObservationsSummaryTable |+| ExploreStyles.ExploreSelectableTable,
        headerCellMod = _ => ExploreStyles.StickyHeader,
        rowMod = row =>
          TagMod(
            ExploreStyles.TableRowSelected
              .when(row.getIsSelected() && (row.subRows.isEmpty || !row.getIsExpanded())),
            ExploreStyles.TableRowSelectedStart
              .when(row.getIsSelected() && row.subRows.nonEmpty && row.getIsExpanded()),
            ExploreStyles.TableRowSelectedSpan
              .when:
                props.selectedObsIds.get.contains_(row.original.value.obs.id)
            ,
            ExploreStyles.TableRowSelectedEnd.when:
              row.original.value.isLastAsterismTargetOf
                .exists(props.selectedObsIds.get.contains_)
            ,
            ^.onClick ==> table
              .getMultiRowSelectedHandler(RowId(row.original.value.obs.id.toString))
          ),
        emptyMessage = <.span(LucumaStyles.HVCenter)(
          Button(
            severity = Button.Severity.Success,
            icon = Icons.New,
            disabled = adding.get.value,
            loading = adding.get.value,
            label = "Add an observation",
            clazz = LucumaPrimeStyles.Massive |+| ExploreStyles.ObservationsSummaryAdd,
            onClick =
              insertObs(props.programId, none, props.observations, adding, ctx).runAsyncAndForget
          ).tiny.compact
        )
      )
  end Body

  case class TileState(
    columnVisibility:      ColumnVisibility,
    toggleAllRowsSelected: Option[Boolean => Callback]
  )

  object TileState:
    val Initial: TileState = TileState(DefaultColVisibility, None)

    val columnVisibility: Lens[TileState, ColumnVisibility]                 =
      Focus[TileState](_.columnVisibility)
    val toggleAllRowsSelected: Lens[TileState, Option[Boolean => Callback]] =
      Focus[TileState](_.toggleAllRowsSelected)

  private case class Title(
    columnVisibility:      View[ColumnVisibility],
    toggleAllRowsSelected: Option[Boolean => Callback]
  ) extends ReactFnProps(Title)
  private object Title
      extends ReactFnComponent[Title](props =>
        React.Fragment(
          props.toggleAllRowsSelected.map: toggleAllRowsSelected =>
            <.span(^.textAlign.center)(
              Button(
                size = Button.Size.Small,
                icon = Icons.CheckDouble,
                label = "All",
                onClick = toggleAllRowsSelected(true)
              ).compact,
              Button(
                size = Button.Size.Small,
                icon = Icons.SquareXMark,
                label = "None",
                onClick = toggleAllRowsSelected(false)
              ).compact
            ),
          ColumnSelectorInTitle(SelectableColumnNames, props.columnVisibility)
        )
      )
