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
import explore.model.Focused
import explore.model.ObsSummaryWithTitleConstraintsAndConf
import explore.model.ObsWithConstraints
import explore.model.enums.AppTab
import explore.model.enums.TableId
import explore.model.reusability.given
import explore.model.syntax.all.*
import explore.shortcuts.GoToSummary
import explore.shortcuts.ShortcutCallbacks
import explore.shortcuts.toHotKeys
import explore.syntax.ui.*
import explore.undo.UndoStacks
import japgolly.scalajs.react.ScalaFnComponent
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.react.syntax.*
import lucuma.react.table.ColumnDef
import lucuma.react.table.ColumnId
import lucuma.react.table.*
import lucuma.typed.tanstackReactTable.tanstackReactTableStrings.columnVisibility
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.TableHooks
import lucuma.ui.table.TableOptionsWithStateStore
import lucuma.ui.table.*
import org.typelevel.log4cats.Logger
import queries.schemas.odb.ObsQueries.ConstraintsList
import queries.schemas.odb.ObsQueries.ObservationList
import react.common.ReactFnProps
import react.hotkeys.*
import react.hotkeys.hooks.*

final case class ObsSummaryTable(
  userId:        Option[User.Id],
  programId:     Program.Id,
  observations:  View[ObservationList],
  obsListStacks: View[UndoStacks[IO, ObservationList]],
  renderInTitle: Tile.RenderInTitle
) extends ReactFnProps(
      ObsSummaryTable.component
    )

object ObsSummaryTable extends TableHooks:
  private type Props = ObsSummaryTable

  private val ColDef = ColumnDef[ObsSummaryWithTitleConstraintsAndConf]

  private val GroupsColumnId          = ColumnId("groups")
  private val ObservationIdColumnId   = ColumnId("observation_id")
  private val ValidationCheckColumnId = ColumnId("validation_check")
  private val StatusColumnId          = ColumnId("status")
  private val CompletionColumnId      = ColumnId("completion")
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

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    // Columns
    .useMemoBy((_, _) => ())((props, ctx) =>
      _ =>
        def column[V](id: ColumnId, accessor: ObsSummaryWithTitleConstraintsAndConf => V)
          : ColumnDef.Single[ObsSummaryWithTitleConstraintsAndConf, V] =
          ColDef(id, accessor, columnNames(id))

        List(
          // TODO: GroupsColumnId
          column(ObservationIdColumnId, _.id).sortable,

          // TODO: ValidationCheckColumnId
          column(StatusColumnId, _.status),
          // TODO: CompletionColumnId
          // TODO: TargetTypeColumnId
          column(TargetTypeColumnId, _ => ())
            .setCell(_ => Icons.Star.withFixedWidth())
            .setSize(35.toPx),
          column(TargetColumnId, _.title),
          column(ConstraintsColumnId, _.constraints).setCell(_.value.summaryString),
          // TODO: FindingChartColumnId
          column(ConfigurationColumnId, _.conf),
          column(DurationColumnId, _.executionTime.toHoursMinutes)

          // TODO: PriorityColumnId
          // TODO: RAColumnId
          // TODO: DecColumnId
          // TODO: TimingColumnId
          // TODO: SEDColumnId
          // TODO: ChargedTimeColumnId
        )
    )
    // Rows
    .useMemoBy((props, _, _) => props.observations.get)((_, _, _) => _.toList)
    // TODO: useReactTableWithStateStoreBy
    .useReactTableBy((props, ctx, cols, rows) =>
      // import ctx.given
      // TableOptionsWithStateStore(
      TableOptions(
        cols,
        rows,
        getRowId = (row, _, _) => RowId(row.id.toString),
        initialState = TableState(columnVisibility = DefaultColVisibility)
      ),
      // TableStore(props.userId, TableId.ObservationsSummary, cols)
      // )
    )
    .render { (props, _, _, _, table) =>
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
          compact = Compact.Very,
          tableMod = ExploreStyles.ExploreTable,
          headerCellMod = _ => ExploreStyles.StickyHeader
        )
      )
    }
