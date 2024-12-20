// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Order
import cats.Order.given
import cats.effect.IO
import cats.syntax.all.*
import crystal.Pot
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.syntax.pot.given
import eu.timepit.refined.cats.given
import explore.Icons
import explore.common.UserPreferencesQueries
import explore.common.UserPreferencesQueries.TableStore
import explore.components.ColumnSelectorInTitle
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Asterism
import explore.model.Execution
import explore.model.Focused
import explore.model.Group
import explore.model.GroupList
import explore.model.ObsSummaryTabTileIds
import explore.model.Observation
import explore.model.ObservationExecutionMap
import explore.model.ObservationList
import explore.model.TargetList
import explore.model.display.given
import explore.model.enums.AppTab
import explore.model.enums.TableId
import explore.model.reusability.given
import explore.model.syntax.all.*
import explore.syntax.ui.*
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.ScalaFnComponent
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Coordinates
import lucuma.core.math.validation.MathValidators
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.syntax.display.*
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.react.table.ColumnDef
import lucuma.react.table.ColumnId
import lucuma.schemas.model.TargetWithId
import lucuma.ui.LucumaStyles
import lucuma.ui.format.TimeSpanFormatter.HoursMinutesAbbreviation
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.table.*
import lucuma.ui.table.*
import lucuma.ui.table.hooks.*
import monocle.Focus
import monocle.Iso
import monocle.Lens

import java.time.Instant
import java.util.UUID
import scala.collection.immutable.TreeSeqMap

object ObsSummaryTile:
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

  private val ObservationIdColumnId = ColumnId("observation_id")
  private val GroupColumnId         = ColumnId("group")
  private val StateColumnId         = ColumnId("state")
  private val ScienceBandColumnId   = ColumnId("science_band")
  private val ExpanderColumnId      = ColumnId("expander")
  private val TargetTypeColumnId    = ColumnId("target_type")
  private val TargetColumnId        = ColumnId("target")
  private val ConstraintsColumnId   = ColumnId("constraints")
  private val ConfigurationColumnId = ColumnId("configuration")
  private val DurationColumnId      = ColumnId("duration")
  private val RAColumnId            = ColumnId("ra")
  private val DecColumnId           = ColumnId("dec")
  private val SEDColumnId           = ColumnId("sed")
  // private val ValidationCheckColumnId = ColumnId("validation_check")
  // private val CompletionColumnId    = ColumnId("completion")
  // private val FindingChartColumnId  = ColumnId("finding_chart")
  // private val PriorityColumnId      = ColumnId("priority")
  // private val TimingWindowsColumnId = ColumnId("timing_windows")
  // private val ChargedTimeColumnId = ColumnId("charged_time")

  private val ColumnNames: TreeSeqMap[ColumnId, String] =
    TreeSeqMap(
      ExpanderColumnId      -> " ",
      ObservationIdColumnId -> "Observation Id",
      TargetTypeColumnId    -> "Target Type",
      TargetColumnId        -> "Targets",
      GroupColumnId         -> "Group",
      StateColumnId         -> "State",
      ScienceBandColumnId   -> "Science Band",
      RAColumnId            -> "RA",
      DecColumnId           -> "Dec",
      SEDColumnId           -> "SED",
      ConstraintsColumnId   -> "Constraints",
      ConfigurationColumnId -> "Configuration",
      DurationColumnId      -> "Duration"
      // ValidationCheckColumnId -> " ",
      // CompletionColumnId    -> "Completion",
      // FindingChartColumnId -> "Finding Chart",
      // Default hidden columns
      // PriorityColumnId      -> "Priority",
      // TimingWindowsColumnId -> "Scheduling Windows",
      // ChargedTimeColumnId -> "ChargedTime"
    )

  private val ColumnsExcludedFromVisibility: Set[ColumnId] =
    Set(ExpanderColumnId)

  private val ColumnHeaderOverrides: Set[ColumnId] =
    Set(TargetTypeColumnId)

  // Columns to be shown in the column visibility selector. We exclude
  // the science band because we set that visibility below.
  private val SelectableColumnNames: List[(ColumnId, String)] =
    ColumnNames.filterNot((k, _) => ColumnsExcludedFromVisibility.contains(k)).toList

  private val DefaultColVisibility: ColumnVisibility =
    ColumnVisibility(
      RAColumnId  -> Visibility.Hidden,
      DecColumnId -> Visibility.Hidden,
      SEDColumnId -> Visibility.Hidden
      // PriorityColumnId      -> Visibility.Hidden,
      // TimingWindowsColumnId -> Visibility.Hidden,
      // ChargedTimeColumnId -> Visibility.Hidden
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

    private type Props = Body

    given Reusability[UUID]                    = Reusability.byEq
    given Reusability[ObservationExecutionMap] = Reusability.by(_.value.toList)

    private val ColDef = ColumnDef[Expandable[ObsSummaryRow]]

    // For columns that only have data in the base observation row.
    private def obsColumn[V](
      id:       ColumnId,
      accessor: ObsRow => V
    ): ColumnDef.Single.NoMeta[Expandable[ObsSummaryRow], Option[V]] =
      ColDef(
        id,
        v => v.value.fold(_ => none, accessor(_).some),
        if (ColumnHeaderOverrides.contains(id)) " " else ColumnNames(id)
      )

    extension [A](name: String | (A, TargetWithId))
      def sortableValue =
        name match
          case s: String => s
          case (_, b)    => b.target.name.value

    extension (a: Option[Pot[Option[TimeSpan]]])
      def sortableValue =
        a.flatMap(_.toOption).flatten

    // Column with expanded accessor. For rows that have data in the expanded target row.
    private def mixedColumn[V](
      id:               ColumnId,
      accessor:         ObsRow => V,
      expandedAccessor: ExpandedTargetRow => V
    ): ColumnDef.Single.NoMeta[Expandable[ObsSummaryRow], V] =
      ColDef(id, v => v.value.fold(expandedAccessor, accessor), ColumnNames(id))

    private val component = ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useMemoBy((_, _) => ()): (props, ctx) => // Columns
        _ =>
          def constraintUrl(constraintId: Observation.Id): String =
            ctx.pageUrl((AppTab.Constraints, props.programId, Focused.singleObs(constraintId)).some)

          def goToConstraint(constraintId: Observation.Id): Callback =
            ctx.pushPage(
              (AppTab.Constraints, props.programId, Focused.singleObs(constraintId)).some
            )

          def targetLink(obsId: Observation.Id, tWId: TargetWithId): VdomNode =
            val text = tWId.target.name.value
            ctx.routingLink(
              (AppTab.Observations, props.programId, Focused.singleObs(obsId, tWId.id.some)).some,
              text
            )

          def obsLink(obsId: Observation.Id): VdomNode =
            ctx.obsIdRoutingLink(props.programId, obsId)

          def groupLink(group: Group): VdomNode =
            val text = group.name.map(_.toString).getOrElse(group.id.toString)
            ctx.routingLink(
              (AppTab.Observations, props.programId, Focused.group(group.id)).some,
              text
            )

          List(
            ColDef(
              ExpanderColumnId,
              cell = cell =>
                if (cell.row.getCanExpand())
                  <.span(
                    ^.cursor.pointer,
                    TableStyles.ExpanderChevron,
                    TableStyles.ExpanderChevronOpen.when(cell.row.getIsExpanded()),
                    ^.onClick ==> (_.stopPropagationCB *> cell.row.getToggleExpandedHandler())
                  )(TableIcons.ChevronRight.withFixedWidth(true))
                else "",
              enableResizing = false
            ).setSize(35.toPx),
            obsColumn(ObservationIdColumnId, _.obs.id)
              .setCell:
                _.value.map(obsLink)
              .sortable,
            // TODO: TargetTypeColumnId
            obsColumn(TargetTypeColumnId, _ => ())
              .setCell(_ => Icons.Star.withFixedWidth())
              .setSize(35.toPx)
              .sortable,
            mixedColumn(
              TargetColumnId,
              r => r.obs.title,
              r => (r.obs.id, r.targetWithId)
            )
              .setCell:
                _.value match
                  case s: String => <.span(s)
                  case (a, b)    => targetLink(a, b)
              .sortableBy(_.sortableValue),
            obsColumn(GroupColumnId, _.group)
              .setCell:
                _.value.flatten.map(groupLink)
              .sortableBy(_.flatMap(_.flatMap(_.name))),
            // TODO: ValidationCheckColumnId
            obsColumn(StateColumnId, _.obs.workflow.state)
              .setCell(_.value.map(_.toString).orEmpty)
              .sortable,
            obsColumn(ScienceBandColumnId, _.obs.scienceBand)
              .setCell:
                _.value.flatten.fold("Not set")(_.shortName)
              .sortable,
            // TODO: CompletionColumnId
            mixedColumn(
              RAColumnId,
              // at visualization time, defaults to base coordinates
              r => r.coordsAtVizTime.map(_.ra),
              r => r.coordsAtVizTime.map(_.ra)
            )
              .setCell(_.value.map(MathValidators.truncatedRA.reverseGet).orEmpty)
              .sortable,
            mixedColumn(
              DecColumnId,
              // at visualization time, defaults to base coordinates
              r => r.coordsAtVizTime.map(_.dec),
              r => r.coordsAtVizTime.map(_.dec)
            )
              .setCell(_.value.map(MathValidators.truncatedDec.reverseGet).orEmpty)
              .sortable,
            // TODO: TimingColumnId
            // TODO: SEDColumnId
            ColDef(
              SEDColumnId,
              v =>
                v.value
                  .fold(_.targetWithId.target.some, _.targetWithId.map(_.target))
                  .flatMap(Target.sidereal.getOption)
                  .flatMap(t =>
                    Target.Sidereal.integratedSpectralDefinition
                      .getOption(t)
                      .orElse(Target.Sidereal.surfaceSpectralDefinition.getOption(t))
                  )
                  .map(_.shortName),
              ColumnNames(SEDColumnId)
            ).setCell(cell =>
              cell.value
                .filterNot(_ => cell.row.getCanExpand())
                .orEmpty
            ).sortable,
            obsColumn(ConstraintsColumnId, r => (r.obs.id, r.obs.constraints.summaryString))
              .setCell: cell =>
                cell.value.map: (id, constraintsSummary) =>
                  <.a(
                    ^.href := constraintUrl(id),
                    ^.onClick ==> (_.preventDefaultCB *> goToConstraint(id)),
                    constraintsSummary
                  )
              .sortableBy(_.map(_._2)),
            // TODO: FindingChartColumnId
            obsColumn(ConfigurationColumnId, _.obs.configurationSummary.orEmpty)
              .setCell(_.value.orEmpty)
              .sortable,
            obsColumn(
              DurationColumnId,
              _.execution.map(_.programTimeEstimate)
            ).setCell:
              _.value.map:
                _.orSpinner(_.map(HoursMinutesAbbreviation.format).orEmpty)
            .sortableBy(_.sortableValue)
            // TODO: PriorityColumnId
            // TODO: ChargedTimeColumnId
          )
      .useMemoBy((props, _, _) => // Rows
        (props.observations.get.values.toList,
         props.allTargets,
         props.groups.get,
         props.obsExecutions
        )
      ): (_, _, _) =>
        (obsList, allTargets, groups, obsExecutions) =>
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
      .useReactTableWithStateStoreBy: (props, ctx, cols, rows) =>
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

        val rowSelection: View[RowSelection] = props.selectedObsIds.as(obsIds2RowSelection)

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
      .useEffectOnMountBy: (props, _, _, _, table) =>
        props.setToggleAllRowsSelected(table.toggleAllRowsSelected)
      .useEffectWithDepsBy((props, _, _, _, _) => props.showScienceBand): (_, _, _, _, table) =>
        showScienceBand =>
          table
            .getColumn(ScienceBandColumnId.value)
            .foldMap(_.toggleVisibility(showScienceBand))
      .useResizeDetector()
      .useStateView(AddingObservation(false)) // adding new observation
      .render: (props, ctx, _, rows, table, resizer, adding) =>
        PrimeAutoHeightVirtualizedTable(
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

  // 24 October 2024 - scalafix failing to parse with fewer braces
  // Helper ADT for table rows type
  enum ObsSummaryRow {
    val obs: Observation

    case ExpandedTargetRow(
      obs:          Observation,
      targetWithId: TargetWithId,
      vizTime:      Option[Instant]
    ) extends ObsSummaryRow

    case ObsRow(
      obs:          Observation,
      targetWithId: Option[TargetWithId],
      asterism:     Option[Asterism],
      group:        Option[Group],
      execution:    Pot[Execution]
    ) extends ObsSummaryRow

    def fold[A](f: ExpandedTargetRow => A, g: ObsRow => A): A =
      this match
        case r: ExpandedTargetRow => f(r)
        case r: ObsRow            => g(r)

    def isLastAsterismTargetOf: Option[Observation.Id] = fold(
      targetRow =>
        Option.when(obs.scienceTargetIds.lastOption.contains_(targetRow.targetWithId.id))(obs.id),
      _ => none
    )

    def coordsAtVizTime: Option[Coordinates] =
      this match
        case r: ExpandedTargetRow => targetCoords(r.targetWithId, r.vizTime)
        case r: ObsRow            =>
          asterismCoords(r.asterism, r.obs.observationTime)
            .orElse(r.targetWithId.flatMap(t => targetCoords(t, r.obs.observationTime)))

    private def targetCoords(twid: TargetWithId, vizTime: Option[Instant]): Option[Coordinates] =
      Target.sidereal
        .getOption(twid.target)
        .flatMap(t => vizTime.fold(t.tracking.baseCoordinates.some)(t.tracking.at))

    private def asterismCoords(
      asterism: Option[Asterism],
      vizTime:  Option[Instant]
    ): Option[Coordinates] =
      asterism
        .map(_.baseTracking)
        .flatMap(bt => vizTime.fold(bt.baseCoordinates.some)(v => bt.at(v).map(_.value)))
  }

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
  ) extends ReactFnProps(Title.component)

  private object Title:
    private type Props = Title

    private val component = ScalaFnComponent[Props]: props =>
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
