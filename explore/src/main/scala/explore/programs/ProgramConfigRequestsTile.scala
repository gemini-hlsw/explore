// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.Order.given
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import crystal.react.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
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
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.Tooltip
import lucuma.react.primereact.tooltip.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.react.syntax.*
import lucuma.react.table.*
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

  case class TileState(table: Option[Table[Row, Nothing, Nothing, Nothing]], selected: List[RowId])

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

    given Reusability[Map[ConfigurationRequest.Id, List[Observation]]] = Reusability.map

    private val ColDef        = ColumnDef[Row]
    private val columnBuilder = ConfigurationTableColumnBuilder(ColDef)

    private val ConfigRequestIdColumnId = ColumnId("config_request_id")
    private val StatusColumnId          = ColumnId("status")

    val ColumnNames: Map[ColumnId, String] = Map(
      ConfigRequestIdColumnId -> "ID",
      StatusColumnId          -> "Status"
    )

    private def rowColumn[V](id: ColumnId, accessor: Row => V): ColDef.TypeFor[V] =
      ColDef(id, accessor, ColumnNames(id))

    private def stateIcon(status: ConfigurationRequestStatus): VdomNode =
      val style = status match
        case ConfigurationRequestStatus.Requested => LucumaStyles.IndicatorWarning
        case ConfigurationRequestStatus.Approved  => LucumaStyles.IndicatorOK
        case ConfigurationRequestStatus.Denied    => LucumaStyles.IndicatorFail
        case ConfigurationRequestStatus.Withdrawn => LucumaStyles.IndicatorUnknown
      <.span(Icons.CircleSolid.withClass(style))
        .withTooltip(content = status.shortName, position = Tooltip.Position.Left)

    val component = ScalaFnComponent[Body](props =>
      for {
        ctx     <- useContext(AppContext.ctx)
        columns <- useMemo(()): _ =>
                     List(
                       rowColumn(ConfigRequestIdColumnId, _.request.id).withSize(90.toPx).sortable,
                       columnBuilder.targetColumn(_.targetName)
                     ) ++
                       columnBuilder.configurationColumns(_.request.configuration) ++
                       List(
                         columnBuilder
                           .obsListColumn(_.observations, props.programId, ctx),
                         rowColumn(StatusColumnId, _.request.status)
                           .withCell(c => stateIcon(c.value))
                           .withSize(80.toPx)
                           .sortable
                       )
        rows    <-
          useMemo((props.configRequests, props.obs4ConfigRequests, props.targets)):
            (requests, obsMap, targets) => requests.map((_, r) => Row(r, obsMap, targets)).toList
        table   <- useReactTableWithStateStore {
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
                   }
        _       <- useEffectOnMount(
                     props.tileState.zoom(TileState.table).set(table.some)
                   )
        resizer <- useResizeDetector
      } yield PrimeAutoHeightVirtualizedTable(
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
    )

  case class Title(
    configRequests: View[ConfigurationRequestList],
    readonly:       Boolean,
    tileState:      TileState
  ) extends ReactFnProps(Title)

  object Title
      extends ReactFnComponent[Title](props =>
        for {
          ctx              <- useContext(AppContext.ctx)
          selectedRequests <-
            useMemo((props.tileState.selected.map(_.value), props.configRequests.get)):
              (selectedIds, requests) =>
                selectedIds
                  .map(rowId => ConfigurationRequest.Id.parse(rowId).flatMap(requests.get))
                  .flattenOption
        } yield
          import ctx.given

          props.tileState.table.map: table =>
            val selectedIds = selectedRequests.value.map(r => r.id)

            def changeStatus(status: ConfigurationRequestStatus): Callback =
              selectedIds.traverse(id =>
                props.configRequests
                  .mod(
                    _.updatedWith(id)(_.map(ConfigurationRequestWithObsIds.status.replace(status)))
                  )
              ) >>
                ProgramQueries
                  .updateConfigurationRequestStatus[IO](selectedIds, status, none)
                  .runAsync

            def changeStatusAndJustification(
              status: ConfigurationRequestStatus
            ): NonEmptyString => Callback = justification =>
              selectedIds.traverse(id =>
                props.configRequests
                  .mod(
                    _.updatedWith(id)(
                      _.map(
                        ConfigurationRequestWithObsIds.status
                          .replace(status)
                          .andThen(
                            ConfigurationRequestWithObsIds.justification.replace(justification.some)
                          )
                      )
                    )
                  )
              ) >>
                ProgramQueries
                  .updateConfigurationRequestStatus[IO](selectedIds, status, justification.some)
                  .runAsync

            def allAreThisStatus(status: ConfigurationRequestStatus) =
              selectedRequests.value.nonEmpty && selectedRequests.forall(_.status === status)

            <.div(ExploreStyles.TableSelectionToolbar)(
              Button(
                icon = Icons.CheckDouble,
                label = "All",
                onClick = table.toggleAllRowsSelected(true)
              ).small.compact,
              Button(
                icon = Icons.SquareXMark,
                label = "None",
                onClick = table.toggleAllRowsSelected(false)
              ).small.compact,
              ConfigurationRequestJustificationViewer(
                requests = selectedRequests,
                trigger = Button(
                  icon = Icons.BookOpen,
                  tooltip = "View Justification Message",
                  disabled = selectedRequests.length === 0
                ).small.compact
              ),
              Option.unless(props.readonly):
                React.Fragment(
                  Button(
                    icon = Icons.PaperPlaneTop,
                    label = "Withdraw Requests",
                    onClick = changeStatus(ConfigurationRequestStatus.Withdrawn),
                    disabled = !allAreThisStatus(ConfigurationRequestStatus.Requested)
                  ).small.compact,
                  ConfigurationRequestEditorPopup(
                    onSubmit = changeStatusAndJustification(ConfigurationRequestStatus.Requested),
                    initialMessages = selectedRequests.value.map(_.justification.foldMap(_.value)),
                    trigger = Button(
                      icon = Icons.PaperPlaneTop,
                      label = "Resubmit Requests",
                      disabled = !allAreThisStatus(ConfigurationRequestStatus.Withdrawn)
                    ).small.compact
                  )
                )
            )
      )
