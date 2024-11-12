// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.syntax.all.*
import explore.Icons
import explore.common.UserPreferencesQueries.TableStore
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ConfigurationRequestList
import explore.model.IsActive
import explore.model.Observation
import explore.model.ObservationList
import explore.model.TargetList
import explore.model.enums.TableId
import explore.model.reusability.given
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Configuration
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.resizeDetector.hooks.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.react.table.ColumnDef
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.table.*
import lucuma.ui.table.*
import lucuma.ui.table.hooks.*
import monocle.Focus
import monocle.Iso
import monocle.Lens
import queries.schemas.odb.ObsQueries
import scala.collection.immutable.SortedMap
import cats.Order.given

object ProgramUnrequestedConfigsTable:
  case class Row(
    configuration: Configuration,
    observations:  NonEmptyList[Observation],
    targetName:    String
  ):
    val id = configuration.toString

  object Row:
    def apply(
      configuration: Configuration,
      observations:  NonEmptyList[Observation],
      targets:       TargetList
    ): Row =
      val targetName = ConfigurationTableColumnBuilder.targetName(observations.toList, targets)
      Row(configuration, observations, targetName)

  case class TileState(table: Option[Table[Row, Nothing]], selected: List[RowId]):
    def selectedRows: List[Row] =
      table.foldMap(t => selected.map(id => t.getRow(id.value).original))

  object TileState:
    val Empty: TileState = TileState(none, List.empty)

    val table    = Focus[TileState](_.table)
    val selected = Focus[TileState](_.selected)

  case class Body(
    userId:                 Option[User.Id],
    programId:              Program.Id,
    configRequests:         View[ConfigurationRequestList],
    configsWithoutRequests: Map[Configuration, NonEmptyList[Observation]],
    targets:                TargetList,
    tileState:              View[TileState]
  ) extends ReactFnProps(Body.component)

  object Body:
    private type Props = Body

    given Reusability[Map[Configuration, NonEmptyList[Observation]]] = Reusability.map

    private val ColDef        = ColumnDef[Row]
    private def columnBuilder = ConfigurationTableColumnBuilder(ColDef)

    val component = ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useMemoBy((_, _) => ()): (props, ctx) => // Columns
        _ =>
          columnBuilder.targetColumn(_.targetName) ::
            (columnBuilder.configurationColumns(_.configuration) :+
              columnBuilder.obsListColumn(_.observations.toList, props.programId, ctx))
      .useMemoBy((props, _, _) => (props.configsWithoutRequests, props.targets)): (_, _, _) =>
        (configs, targets) => configs.toList.map((c, os) => Row(c, os, targets))
      .useReactTableWithStateStoreBy: (props, ctx, columns, rows) =>
        import ctx.given

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

        val rowSelection: View[RowSelection] =
          props.tileState.zoom(TileState.selected).as(rowIds2RowSelection)

        TableOptionsWithStateStore(
          TableOptions(
            columns,
            rows,
            getRowId = (row, _, _) => RowId(row.id),
            enableMultiRowSelection = true,
            state = PartialTableState(
              rowSelection = rowSelection.get
            ),
            onRowSelectionChange = stateInViewHandler(rowSelection.mod)
          ),
          TableStore(
            props.userId,
            TableId.UnrequestedConfigs,
            columns
          )
        )
      .useEffectOnMountBy((props, _, _, _, table) =>
        props.tileState.zoom(TileState.table).set(table.some)
      )
      .useResizeDetector()
      .render: (props, ctx, _, rows, table, resizer) =>
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
                .getMultiRowSelectedHandler(RowId(row.original.id))
            ),
          emptyMessage = <.div("There are no observations without requests.")
        )

  case class Title(
    configRequests: View[ConfigurationRequestList],
    observations:   View[ObservationList],
    tileState:      TileState
  ) extends ReactFnProps(Title.component)

  object Title:
    private type Props = Title

    private val component = ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView(IsActive(false))
      .render: (props, ctx, isActive) =>
        import ctx.given

        def submitOne(row: Row): IO[Unit] =
          ObsQueries
            .createConfigurationRequest[IO](row.observations.head.id)
            .flatMap: request =>
              (props.configRequests.mod(_.updated(request.id, request)) >>
                props.observations.mod: obsList =>
                  SortedMap
                    .from:
                      obsList.mapValues:
                        _.updateToPendingIfConfigurationApplies(request.configuration)
              ).to[IO]

        props.tileState.table.map: table =>
          def submitRequests(msg: String): Callback =
            CallbackTo(props.tileState.selectedRows)
              .flatMap(
                _.traverse_(submitOne)
                  .switching(isActive.async, IsActive(_))
                  .runAsync
              ) >>
              table.toggleAllRowsSelected(false)

          React.Fragment(
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
            ConfigurationRequestEditorPopup(
              onSubmit = submitRequests,
              trigger = Button(
                size = Button.Size.Small,
                icon = Icons.PaperPlaneTop,
                label = "Request Approval",
                disabled = props.tileState.selected.isEmpty || isActive.get.value
              ).compact
            )
          )
