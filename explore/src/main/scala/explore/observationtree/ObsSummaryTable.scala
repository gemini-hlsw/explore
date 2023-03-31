// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import crystal.react.reuse.*
import explore.Icons
import explore.common.ConstraintGroupQueries.*
import explore.common.UserPreferencesQueries.TableStore
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Asterism
import explore.model.Focused
import explore.model.ObsIdSet
import explore.model.ObsSummary
import explore.model.ObsWithConstraints
import explore.model.display.given
import explore.model.enums.AppTab
import explore.model.enums.TableId
import explore.model.reusability.given
import explore.model.syntax.all.*
import explore.shortcuts.GoToSummary
import explore.shortcuts.ShortcutCallbacks
import explore.shortcuts.toHotKeys
import explore.syntax.ui.*
import japgolly.scalajs.react.ScalaFnComponent
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.TagOf
import japgolly.scalajs.react.vdom.html_<^.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.validation.MathValidators
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.syntax.display.*
import lucuma.react.syntax.*
import lucuma.react.table.ColumnDef
import lucuma.react.table.ColumnId
import lucuma.react.table.*
import lucuma.schemas.model.TargetWithId
import lucuma.typed.tanstackReactTable.tanstackReactTableStrings.columnVisibility
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.TableHooks
import lucuma.ui.table.TableOptionsWithStateStore
import lucuma.ui.table.*
import org.scalajs.dom.html.Anchor
import org.typelevel.log4cats.Logger
import queries.schemas.odb.ObsQueries.ObservationList
import react.common.ReactFnProps
import react.hotkeys.*
import react.hotkeys.hooks.*

import scala.collection.immutable.SortedMap
import scala.scalajs.js
import explore.model.TargetWithObs

final case class ObsSummaryTable(
  userId:        Option[User.Id],
  programId:     Program.Id,
  observations:  View[ObservationList],
  targetsMap:    View[SortedMap[Target.Id, TargetWithObs]],
  renderInTitle: Tile.RenderInTitle
) extends ReactFnProps(ObsSummaryTable.component)

object ObsSummaryTable extends TableHooks:
  import ObsSummaryRow.*

  private type Props = ObsSummaryTable

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
    TimingWindowsColumnId -> "Timing Windows",
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

  private def column[V](
    id:       ColumnId,
    accessor: ObsRow => V
  ): ColumnDef.Single[Expandable[ObsSummaryRow], js.UndefOr[V]] =
    ColDef(id, v => v.value.fold(_ => js.undefined, accessor), columnNames(id))

  // Column with expanded accessor. For rows that have data in the expanded target row.
  private def column[V](
    id:               ColumnId,
    accessor:         ObsRow => V,
    expandedAccessor: ExpandedTargetRow => V
  ): ColumnDef.Single[Expandable[ObsSummaryRow], V] =
    ColDef(id, v => v.value.fold(expandedAccessor, accessor), columnNames(id))

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    // Columns
    .useMemoBy((_, _) => ()) { (props, ctx) => _ =>
      def constraintUrl(constraintId: Observation.Id): String =
        ctx.pageUrl(AppTab.Constraints, props.programId, Focused.singleObs(constraintId))

      def goToConstraint(constraintId: Observation.Id): Callback =
        ctx.pushPage(AppTab.Constraints, props.programId, Focused.singleObs(constraintId))

      def targetUrl(obsId: Observation.Id, tWId: TargetWithId) = <.a(
        ^.href := ctx.pageUrl(AppTab.Observations,
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
                ExploreStyles.ExpanderChevron,
                ExploreStyles.ExpanderChevronOpen.when(cell.row.getIsExpanded()),
                ^.onClick ==> (_.stopPropagationCB *> Callback(
                  cell.row.getToggleExpandedHandler()()
                ))
              )(Icons.ChevronRightLight.withFixedWidth(true))
            else "",
          enableResizing = false
        ).setSize(35.toPx),
        column(ObservationIdColumnId, _.obs.id),
        // TODO: ValidationCheckColumnId
        column(StatusColumnId, _.obs.status),
        // TODO: CompletionColumnId
        // TODO: TargetTypeColumnId
        column(TargetTypeColumnId, _ => ())
          .setCell(_ => Icons.Star.withFixedWidth())
          .setSize(35.toPx),
        column(TargetColumnId, r => <.span(r.obs.title), r => targetUrl(r.obsId, r.targetWithId))
          .setCell(_.value),
        column(
          RAColumnId,
          r =>
            r.asterism
              .map(_.baseTracking.baseCoordinates.ra) // TODO: baseTrackingAt
              .orElse(r.targetWithId.map(_.target).flatMap(Target.baseRA.getOption)),
          r => Target.baseRA.getOption(r.targetWithId.target)
        )
          .setCell(_.value.map(MathValidators.truncatedRA.reverseGet).orEmpty)
          .sortable,
        column(
          DecColumnId,
          v =>
            v.asterism
              .map(_.baseTracking.baseCoordinates.dec)
              .orElse(v.targetWithId.map(_.target).flatMap(Target.baseDec.getOption)),
          r => Target.baseDec.getOption(r.targetWithId.target)
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
        column(ConstraintsColumnId, r => (r.obs.id, r.obs.constraints.summaryString))
          .setCell(cell =>
            cell.value.map((id, constraintsSummary) =>
              <.a(
                ^.href := constraintUrl(id),
                ^.onClick ==> (_.preventDefaultCB *> goToConstraint(id)),
                constraintsSummary
              )
            )
          )
          .sortableBy(_.toOption.map(_._2)),
        // TODO: FindingChartColumnId
        column(ConfigurationColumnId, _.obs.conf),
        column(DurationColumnId, _.obs.executionTime.toHoursMinutes)
        // TODO: PriorityColumnId
        // TODO: ChargedTimeColumnId
      )
    }
    // Rows
    .useMemoBy((props, _, _) => (props.observations.get.toList, props.targetsMap.get))((_, _, _) =>
      (obsList, targetsMap) =>
        obsList.toList
          .map(obs =>
            obs -> targetsMap
              .filter((_, target) => target.obsIds.contains(obs.id))
              .map((id, target) => TargetWithId(id, target.target))
              .toList
          )
          .map((obs, targets) =>
            val asterism = Asterism.fromTargets(targets)
            Expandable(
              ObsRow(obs, targets.headOption, asterism),
              // Only expand if there are multiple targets
              if (targets.sizeIs > 1)
                targets.map(target => Expandable(ExpandedTargetRow(obs.id, target)))
              else Nil
            )
          )
    )
    .useReactTableWithStateStoreBy((props, ctx, cols, rows) =>
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
    )
    .render { (props, ctx, _, _, table) =>
      import ctx.given

      <.div(
        props.renderInTitle(
          React.Fragment(
            <.span(), // Push column selector to right
            <.span(ExploreStyles.TitleSelectColumns)(
              ColumnSelector(table, columnNames, ExploreStyles.SelectColumns)
            )
          )
        ),
        PrimeTable(
          table,
          striped = true,
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
          compact = Compact.Very,
          tableMod = ExploreStyles.ExploreTable,
          headerCellMod = _ => ExploreStyles.StickyHeader
        )
      )
    }

  // Helper ADT for table rows type
  enum ObsSummaryRow(obsId: Observation.Id):

    case ExpandedTargetRow(obsId: Observation.Id, targetWithId: TargetWithId)
        extends ObsSummaryRow(obsId)
    case ObsRow(
      obs:          ObsSummary,
      targetWithId: Option[TargetWithId],
      asterism:     Option[Asterism]
    ) extends ObsSummaryRow(obs.id)

    def fold[A](f: ExpandedTargetRow => A, g: ObsRow => A): A =
      this match
        case r: ExpandedTargetRow => f(r)
        case r: ObsRow            => g(r)
