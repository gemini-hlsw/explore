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
import explore.Icons
import explore.common.UserPreferencesQueries
import explore.common.UserPreferencesQueries.TableStore
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Asterism
import explore.model.Execution
import explore.model.Focused
import explore.model.Observation
import explore.model.ObservationExecutionMap
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
import lucuma.refined.*
import lucuma.schemas.model.TargetWithId
import lucuma.ui.format.TimeSpanFormatter.HoursMinutesAbbreviation
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.table.*
import lucuma.ui.table.hooks.*
import queries.schemas.odb.ObsQueries.ObservationList

import java.time.Instant
import java.util.UUID

final case class ObsSummaryTable(
  userId:        Option[User.Id],
  programId:     Program.Id,
  observations:  UndoSetter[ObservationList],
  obsExecutions: ObservationExecutionMap,
  allTargets:    TargetList,
  renderInTitle: Tile.RenderInTitle
) extends ReactFnProps(ObsSummaryTable.component)

object ObsSummaryTable:
  import ObsSummaryRow.*

  private type Props = ObsSummaryTable

  given Reusability[UUID]                    = Reusability.byEq
  given Reusability[ObservationExecutionMap] = Reusability.by(_.value.toList)

  private val ColDef = ColumnDef[Expandable[ObsSummaryRow]]

  private val GroupsColumnId          = ColumnId("groups")
  private val ObservationIdColumnId   = ColumnId("observation_id")
  private val ValidationCheckColumnId = ColumnId("validation_check")
  private val StatusColumnId          = ColumnId("status")
  private val CompletionColumnId      = ColumnId("completion")
  private val ExpanderColumnId        = ColumnId("expander")
  private val TargetTypeColumnId      = ColumnId("target_type")
  private val TargetColumnId          = ColumnId("target")
  private val ConstraintsColumnId     = ColumnId("constraints")
  private val FindingChartColumnId    = ColumnId("finding_chart")
  private val ConfigurationColumnId   = ColumnId("configuration")
  private val DurationColumnId        = ColumnId("duration")

  private val PriorityColumnId      = ColumnId("priority")
  private val RAColumnId            = ColumnId("ra")
  private val DecColumnId           = ColumnId("dec")
  private val TimingWindowsColumnId = ColumnId("timing_windows")
  private val SEDColumnId           = ColumnId("sed")
  private val ChargedTimeColumnId   = ColumnId("charged_time")

  private val columnNames: Map[ColumnId, String] = Map(
    // Default columns
    GroupsColumnId          -> "Groups",
    ObservationIdColumnId   -> "Observation Id",
    ValidationCheckColumnId -> " ",
    StatusColumnId          -> "Status",
    CompletionColumnId      -> "Completion",
    ExpanderColumnId        -> " ",
    TargetTypeColumnId      -> " ",
    TargetColumnId          -> "Target",
    ConstraintsColumnId     -> "Constraints",
    FindingChartColumnId    -> "Finding Chart",
    ConfigurationColumnId   -> "Configuration",
    DurationColumnId        -> "Duration",

    // Default hidden columns
    PriorityColumnId      -> "Priority",
    RAColumnId            -> "RA",
    DecColumnId           -> "Dec",
    TimingWindowsColumnId -> "Scheduling Windows",
    SEDColumnId           -> "SED",
    ChargedTimeColumnId   -> "ChargedTime"
  )

  private val DefaultColVisibility: ColumnVisibility = ColumnVisibility(
    PriorityColumnId      -> Visibility.Hidden,
    RAColumnId            -> Visibility.Hidden,
    DecColumnId           -> Visibility.Hidden,
    TimingWindowsColumnId -> Visibility.Hidden,
    SEDColumnId           -> Visibility.Hidden,
    ChargedTimeColumnId   -> Visibility.Hidden
  )

  // For columns that only have data in the base observation row.
  private def obsColumn[V](
    id:       ColumnId,
    accessor: ObsRow => V
  ): ColumnDef.Single.NoMeta[Expandable[ObsSummaryRow], Option[V]] =
    ColDef(id, v => v.value.fold(_ => none, accessor(_).some), columnNames(id))

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
    ColDef(id, v => v.value.fold(expandedAccessor, accessor), columnNames(id))

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useMemoBy((_, _) => ()): (props, ctx) => // Columns
      _ =>
        def constraintUrl(constraintId: Observation.Id): String =
          ctx.pageUrl(AppTab.Constraints, props.programId, Focused.singleObs(constraintId))

        def goToConstraint(constraintId: Observation.Id): Callback =
          ctx.pushPage(AppTab.Constraints, props.programId, Focused.singleObs(constraintId))

        def targetUrl(obsId: Observation.Id, tWId: TargetWithId) = <.a(
          ^.href := ctx.pageUrl(
            AppTab.Observations,
            props.programId,
            Focused.singleObs(obsId, tWId.id.some)
          ),
          ^.onClick ==> (e =>
            e.preventDefaultCB *> e.stopPropagationCB *> ctx.pushPage(
              AppTab.Observations,
              props.programId,
              Focused.singleObs(obsId, tWId.id.some)
            )
          ),
          tWId.target.name.value
        )

        List(
          // TODO: GroupsColumnId
          ColDef(
            ColumnId("expander"),
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
          obsColumn(ObservationIdColumnId, _.obs.id).setCell(_.value.map(_.toString).orEmpty),
          // TODO: ValidationCheckColumnId
          obsColumn(StatusColumnId, _.obs.status).setCell(_.value.map(_.toString).orEmpty),
          // TODO: CompletionColumnId
          // TODO: TargetTypeColumnId
          obsColumn(TargetTypeColumnId, _ => ())
            .setCell(_ => Icons.Star.withFixedWidth())
            .setSize(35.toPx),
          mixedColumn(
            TargetColumnId,
            r => r.obs.title,
            r => (r.obsId, r.targetWithId)
          )
            .setCell { c =>
              c.value match {
                case s: String => <.span(s)
                case (a, b)    => targetUrl(a, b)
              }
            }
            .sortableBy(_.sortableValue),
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
            columnNames(SEDColumnId)
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
            .setCell(_.value.orEmpty),
          obsColumn(
            DurationColumnId,
            _.execution.map(_.programTimeEstimate)
          ).setCell { cell =>
            cell.value.map:
              _.orSpinner(_.map(HoursMinutesAbbreviation.format).orEmpty)
          }.sortableBy(_.sortableValue)
          // TODO: PriorityColumnId
          // TODO: ChargedTimeColumnId
        )
    .useMemoBy((props, _, _) => // Rows
      (props.observations.get.toList, props.allTargets, props.obsExecutions)
    ): (_, _, _) =>
      (obsList, allTargets, obsExecutions) =>
        obsList
          .filterNot(_.isCalibration)
          .map: obs =>
            obs -> obs.scienceTargetIds.toList
              .map(id => allTargets.get(id).map(t => TargetWithId(id, t)))
              .flattenOption
          .map: (obs, targets) =>
            val asterism = Asterism.fromTargets(targets)
            Expandable(
              ObsRow(obs, targets.headOption, asterism, obsExecutions.getPot(obs.id)),
              // Only expand if there are multiple targets
              if (targets.sizeIs > 1)
                targets.map: target =>
                  Expandable(ExpandedTargetRow(obs.id, target, obs.visualizationTime))
              else Nil
            )
    .useReactTableWithStateStoreBy: (props, ctx, cols, rows) =>
      import ctx.given

      TableOptionsWithStateStore(
        TableOptions(
          cols,
          rows,
          enableExpanding = true,
          getSubRows = (row, _) => row.subRows,
          getRowId = (row, _, _) =>
            RowId(
              row.value.fold(
                o => o.obsId.toString + o.targetWithId.id.toString,
                _.obs.id.toString
              )
            ),
          initialState = TableState(columnVisibility = DefaultColVisibility)
        ),
        TableStore(props.userId, TableId.ObservationsSummary, cols)
      )
    .useResizeDetector()
    // adding new observation
    .useStateView(AddingObservation(false))
    .render: (props, ctx, _, rows, table, resizer, adding) =>
      React.Fragment(
        props.renderInTitle(
          React.Fragment(
            <.span(), // Push column selector to right
            <.span(ExploreStyles.TitleSelectColumns)(
              ColumnSelector(table, columnNames, ExploreStyles.SelectColumns)
            )
          )
        ),
        PrimeAutoHeightVirtualizedTable(
          table,
          _ => 32.toPx,
          striped = true,
          compact = Compact.Very,
          innerContainerMod = ^.width := "100%",
          containerRef = resizer.ref,
          hoverableRows = rows.nonEmpty,
          tableMod = ExploreStyles.ExploreTable |+| ExploreStyles.ObservationsSummaryTable,
          headerCellMod = _ => ExploreStyles.StickyHeader,
          rowMod = row =>
            TagMod(
              ExploreStyles.CursorPointer,
              ExploreStyles.TableRowSelected.when(row.getIsSelected()),
              ^.role := "link",
              ^.onClick ==> { (e: ReactMouseEvent) =>
                val (obsId, targetId) = row.original.value
                  .fold(o => (o.obsId, o.targetWithId.id.some), o => (o.obs.id, none))
                e.preventDefaultCB *> ctx.pushPage(
                  AppTab.Observations,
                  props.programId,
                  Focused.singleObs(obsId, targetId)
                )
              }
            ),
          emptyMessage = <.span(
            ExploreStyles.HVCenter,
            Button(
              severity = Button.Severity.Success,
              icon = Icons.New,
              disabled = adding.get.value,
              loading = adding.get.value,
              label = "Add an observation",
              clazz = LucumaPrimeStyles.Massive |+| ExploreStyles.ObservationsSummaryAdd,
              onClick = insertObs(
                props.programId,
                none,
                0.refined,
                props.observations,
                adding,
                ctx
              ).runAsyncAndForget
            ).tiny.compact
          )
        )
      )

  // Helper ADT for table rows type
  enum ObsSummaryRow:
    case ExpandedTargetRow(
      obsId:        Observation.Id,
      targetWithId: TargetWithId,
      vizTime:      Option[Instant]
    ) extends ObsSummaryRow

    case ObsRow(
      obs:          Observation,
      targetWithId: Option[TargetWithId],
      asterism:     Option[Asterism],
      execution:    Pot[Execution]
    ) extends ObsSummaryRow

    def fold[A](f: ExpandedTargetRow => A, g: ObsRow => A): A =
      this match
        case r: ExpandedTargetRow => f(r)
        case r: ObsRow            => g(r)

    def coordsAtVizTime: Option[Coordinates] =
      this match
        case r: ExpandedTargetRow => targetCoords(r.targetWithId, r.vizTime)
        case r: ObsRow            =>
          asterismCoords(r.asterism, r.obs.visualizationTime)
            .orElse(r.targetWithId.flatMap(t => targetCoords(t, r.obs.visualizationTime)))

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
