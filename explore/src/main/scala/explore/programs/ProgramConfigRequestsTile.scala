// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.Order.given
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import crystal.react.syntax.all.*
import explore.Icons
import explore.common.ProgramQueries
import explore.common.UserPreferencesQueries.TableStore
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ConfigurationRequestList
import explore.model.ConfigurationRequestWithObsIds
import explore.model.Observation
import explore.model.TargetList
import explore.model.display.given
import explore.model.enums.TableId
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.core.syntax.all.*
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Tooltip
import lucuma.react.primereact.tooltip.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.react.table.ColumnDef
import lucuma.react.table.ColumnId
import lucuma.ui.LucumaStyles
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.table.*
import lucuma.ui.table.*
import lucuma.ui.table.hooks.*
import monocle.Focus
import monocle.Iso
import monocle.Lens

object ProgramConfigRequestsTile:
  case class Row(
    request:      ConfigurationRequestWithObsIds,
    observations: List[Observation],
    targetName:   String
  )

  object Row:
    def apply(
      request:            ConfigurationRequestWithObsIds,
      obs4ConfigRequests: Map[ConfigurationRequest.Id, List[Observation]],
      targets:            TargetList
    ): Row =
      val obses      = obs4ConfigRequests.get(request.id).getOrElse(List.empty)
      val targetName = ConfigurationTableColumnBuilder.targetName(obses, targets)
      Row(request, obses, targetName)

  case class TileState(table: Option[Table[Row, Nothing]], selected: List[RowId]):
    def selectedRows: List[Row] =
      table.foldMap(t => selected.map(id => t.getRow(id.value).original))

  object TileState:
    val Empty: TileState = TileState(none, List.empty)
    val table            = Focus[TileState](_.table)
    val selected         = Focus[TileState](_.selected)

  val rowIds2RowSelection: Iso[List[RowId], RowSelection] =
    Iso[List[RowId], RowSelection](rowIds =>
      RowSelection:
        rowIds.map(_ -> true).toMap
    )(selection =>
      selection.value
        .filter(_._2)
        .keys
        .toList
    )

  case class Body(
    userId:             Option[User.Id],
    programId:          Program.Id,
    configRequests:     ConfigurationRequestList,
    obs4ConfigRequests: Map[ConfigurationRequest.Id, List[Observation]],
    targets:            TargetList,
    tileState:          View[TileState]
  ) extends ReactFnProps(Body.component)

  object Body:

    private type Props = Body

    given Reusability[Map[ConfigurationRequest.Id, List[Observation]]] = Reusability.map

    private val ColDef        = ColumnDef[Row]
    private val columnBuilder = ConfigurationTableColumnBuilder(ColDef)

    private val ConfigRequestIdColumnId = ColumnId("config_request_id")
    private val StatusColumnId          = ColumnId("status")

    val ColumnNames: Map[ColumnId, String] = Map(
      ConfigRequestIdColumnId -> "ID",
      StatusColumnId          -> "Status"
    )

    private def rowColumn[V](
      id:       ColumnId,
      accessor: Row => V
    ): ColumnDef.Single.NoMeta[Row, V] = ColDef(id, accessor, ColumnNames(id))

    private def stateIcon(status: ConfigurationRequestStatus): VdomNode =
      val style = status match
        case ConfigurationRequestStatus.Requested => LucumaStyles.IndicatorWarning
        case ConfigurationRequestStatus.Approved  => LucumaStyles.IndicatorOK
        case ConfigurationRequestStatus.Denied    => LucumaStyles.IndicatorFail
        case ConfigurationRequestStatus.Withdrawn => LucumaStyles.IndicatorUnknown
      <.span(Icons.CircleSolid.withClass(style))
        .withTooltip(content = status.shortName, position = Tooltip.Position.Left)

    val component = ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useMemoBy((_, _) => ()): (props, ctx) => // Columns
        _ =>
          List(
            rowColumn(ConfigRequestIdColumnId, _.request.id).setSize(90.toPx).sortable,
            columnBuilder.targetColumn(_.targetName)
          ) ++
            columnBuilder.configurationColumns(_.request.configuration) ++
            List(
              columnBuilder
                .obsListColumn(_.observations, props.programId, ctx),
              rowColumn(StatusColumnId, _.request.status)
                .setCell(c => stateIcon(c.value))
                .setSize(80.toPx)
                .sortable
            )
      .useMemoBy((props, _, _) => // Rows
        (props.configRequests, props.obs4ConfigRequests, props.targets)
      ): (_, _, _) =>
        (requests, obsMap, targets) => requests.map((_, r) => Row(r, obsMap, targets)).toList
      .useReactTableWithStateStoreBy: (props, ctx, columns, rows) =>
        import ctx.given

        val rowSelection: View[RowSelection] =
          props.tileState.zoom(TileState.selected).as(rowIds2RowSelection)

        TableOptionsWithStateStore(
          TableOptions(
            columns,
            rows,
            getRowId = (row, _, _2) => RowId(row.request.id.toString),
            enableMultiRowSelection = true,
            state = PartialTableState(
              rowSelection = rowSelection.get
            ),
            onRowSelectionChange = stateInViewHandler(rowSelection.mod)
          ),
          TableStore(
            props.userId,
            TableId.RequestedConfigs,
            columns
          )
        )
      .useEffectOnMountBy((props, _, _, _, table) =>
        props.tileState.zoom(TileState.table).set(table.some)
      )
      .useResizeDetector()
      .render { (props, _, _, rows, table, resizer) =>
        PrimeAutoHeightVirtualizedTable(
          table,
          _ => 32.toPx,
          striped = true,
          compact = Compact.Very,
          innerContainerMod = ^.width := "100%",
          containerRef = resizer.ref,
          tableMod = ExploreStyles.ExploreTable |+| ExploreStyles.ExploreSelectableTable,
          hoverableRows = rows.nonEmpty,
          rowMod = row =>
            TagMod(
              ExploreStyles.TableRowSelected.when(row.getIsSelected()),
              ^.onClick ==> table
                .getMultiRowSelectedHandler(RowId(row.original.request.id.toString))
            ),
          emptyMessage = <.div("There are no requests.")
        )
      }

  case class Title(
    configRequests: View[ConfigurationRequestList],
    readonly:       Boolean,
    tileState:      TileState
  ) extends ReactFnProps(Title.component)

  object Title:
    private type Props = Title

    private val component = ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .render: (props, ctx) =>
        import ctx.given

        props.tileState.table.map: table =>
          val selectedRows = props.tileState.selectedRows
          val selectedIds  = selectedRows.map(_.request.id)

          def changeStatus(status: ConfigurationRequestStatus): Callback =
            selectedIds.traverse(id =>
              props.configRequests
                .mod(
                  _.updatedWith(id)(_.map(ConfigurationRequestWithObsIds.status.replace(status)))
                )
            ) >>
              ProgramQueries
                .updateConfigurationRequestStatus[IO](selectedIds, status)
                .runAsync

          def allAreThisStatus(status: ConfigurationRequestStatus) =
            selectedRows.nonEmpty && selectedRows.forall(_.request.status === status)

          Option
            .unless(props.readonly):
              <.div(ExploreStyles.TableSelectionToolbar)(
                Button(
                  size = Button.Size.Small,
                  icon = Icons.CheckDouble,
                  label = "All",
                  onClick = table.toggleAllRowsSelected(true)
                ).compact,
                Button(
                  size = Button.Size.Small,
                  icon = Icons.SquareXMark,
                  label = "None",
                  onClick = table.toggleAllRowsSelected(false)
                ).compact,
                Button(
                  size = Button.Size.Small,
                  icon = Icons.PaperPlaneTop,
                  label = "Withdraw Requests",
                  onClick = changeStatus(ConfigurationRequestStatus.Withdrawn),
                  disabled = !allAreThisStatus(ConfigurationRequestStatus.Requested)
                ).compact,
                Button(
                  size = Button.Size.Small,
                  icon = Icons.PaperPlaneTop,
                  label = "Resubmit Requests",
                  onClick = changeStatus(ConfigurationRequestStatus.Requested),
                  disabled = !allAreThisStatus(ConfigurationRequestStatus.Withdrawn)
                ).compact
              )
