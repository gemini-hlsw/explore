// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targets

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import explore.Icons
import explore.common.UserPreferencesQueries
import explore.common.UserPreferencesQueries.TableStore
import explore.components.ColumnSelectorInTitle
import explore.components.HelpIcon
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Focused
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.TargetList
import explore.model.enums.AppTab
import explore.model.enums.TableId
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.NewType
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.primereact.PrimeStyles
import lucuma.react.resizeDetector.hooks.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.schemas.model.TargetWithId
import lucuma.typed.tanstackVirtualCore as rawVirtual
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import lucuma.ui.syntax.table.*
import lucuma.ui.table.*
import lucuma.ui.table.hooks.*
import monocle.Focus
import org.scalajs.dom.File as DOMFile

import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeSeqMap

object TargetSummaryTile:
  def apply(
    userId:                  Option[User.Id],
    programId:               Program.Id,
    targets:                 View[TargetList],
    targetObservations:      Map[Target.Id, SortedSet[Observation.Id]],
    calibrationObservations: Set[Observation.Id],
    selectObservation:       (Observation.Id, Target.Id) => Callback,
    selectedTargetIds:       View[List[Target.Id]],
    focusedTargetId:         Option[Target.Id],
    focusTargetId:           Option[Target.Id] => Callback,
    readonly:                Boolean,
    backButton:              VdomNode
  ): Tile[TargetSummaryTile.TileState] =
    Tile(
      ObsTabTileIds.TargetSummaryId.id,
      s"Target Summary (${targets.get.size})",
      TileState.Initial,
      backButton.some
    )(
      tileState =>
        Body(
          userId,
          programId,
          targets,
          targetObservations,
          calibrationObservations,
          selectObservation,
          selectedTargetIds,
          focusedTargetId,
          focusTargetId,
          tileState.get.filesToImport.size,
          tileState.get.columnVisibility,
          tileState.zoom(TileState.toggleAllRowsSelected).set.compose(_.some)
        ),
      (tileState, _) =>
        Title(
          programId,
          readonly,
          tileState.zoom(TileState.filesToImport),
          tileState.zoom(TileState.columnVisibility),
          tileState.get.toggleAllRowsSelected
        )
    )

  case class TileState(
    filesToImport:         List[DOMFile],
    columnVisibility:      ColumnVisibility,
    toggleAllRowsSelected: Option[Boolean => Callback]
  )

  object TileState:
    val Initial: TileState = TileState(List.empty, TargetColumns.DefaultVisibility, none)

    val filesToImport         = Focus[TileState](_.filesToImport)
    val columnVisibility      = Focus[TileState](_.columnVisibility)
    val toggleAllRowsSelected = Focus[TileState](_.toggleAllRowsSelected)

  private case class Body(
    userId:                   Option[User.Id],
    programId:                Program.Id,
    targets:                  View[TargetList],
    targetObservations:       Map[Target.Id, SortedSet[Observation.Id]],
    calibrationObservations:  Set[Observation.Id],
    selectObservation:        (Observation.Id, Target.Id) => Callback,
    selectedTargetIds:        View[List[Target.Id]],
    focusedTargetId:          Option[Target.Id],
    focusTargetId:            Option[Target.Id] => Callback,
    filesToImportCount:       Int,
    columnVisibility:         ColumnVisibility,
    setToggleAllRowsSelected: (Boolean => Callback) => Callback
  ) extends ReactFnProps(Body.component)

  private object Body:
    private type Props = Body

    private val ColDef = ColumnDef[TargetWithId]

    private val IdColumnId: ColumnId           = ColumnId("id")
    private val CountColumnId: ColumnId        = ColumnId("count")
    private val ObservationsColumnId: ColumnId = ColumnId("observations")

    private object IsImportOpen extends NewType[Boolean]

    private val ColumnClasses: Map[ColumnId, Css] = Map(
      IdColumnId                 -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryId),
      TargetColumns.TypeColumnId -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryType),
      TargetColumns.NameColumnId -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryName)
    )

    val ColNames: TreeSeqMap[ColumnId, String] =
      TreeSeqMap(
        IdColumnId -> "Id"
      ) ++
        TargetColumns.AllColNames ++
        TreeSeqMap(
          CountColumnId        -> "Count",
          ObservationsColumnId -> "Observations"
        )

    private val ScrollOptions =
      rawVirtual.mod
        .ScrollToOptions()
        .setBehavior(rawVirtual.mod.ScrollBehavior.smooth)
        .setAlign(rawVirtual.mod.ScrollAlignment.center)

    private given Reusability[Map[Target.Id, SortedSet[Observation.Id]]] = Reusability.map

    private val component =
      ScalaFnComponent
        .withHooks[Props]
        .useContext(AppContext.ctx)
        .useMemoBy((_, _) => ()): (props, ctx) => // cols
          _ =>
            def column[V](id: ColumnId, accessor: TargetWithId => V) =
              ColDef(id, row => accessor(row), ColNames(id))

            def obsUrl(targetId: Target.Id, obsId: Observation.Id): String =
              ctx.pageUrl(AppTab.Targets, props.programId, Focused.singleObs(obsId, targetId.some))

            List(
              ColDef(
                IdColumnId,
                _.id,
                "id",
                cell =>
                  <.a(
                    ^.href := ctx.pageUrl(
                      AppTab.Targets,
                      props.programId,
                      Focused.target(cell.value)
                    ),
                    ^.onClick ==> (e =>
                      e.preventDefaultCB >> e.stopPropagationCB >>
                        props.focusTargetId(cell.value.some)
                    )
                  )(
                    cell.value.toString
                  )
              ).sortable
            ) ++
              TargetColumns.Builder.ForProgram(ColDef, _.target.some).AllColumns ++
              List(
                column(
                  CountColumnId,
                  x => props.targetObservations.get(x.id).map(_.size).orEmpty
                ) // TODO Right align
                  .setCell(_.value.toString),
                column(
                  ObservationsColumnId,
                  x => (x.id, props.targetObservations.get(x.id).orEmpty.toList)
                )
                  .setCell(cell =>
                    val (tid, obsIds) = cell.value
                    <.span(
                      obsIds
                        .map(obsId =>
                          <.a(
                            ^.href := obsUrl(tid, obsId),
                            ^.onClick ==> (e =>
                              e.preventDefaultCB >> e.stopPropagationCB >>
                                props.selectObservation(obsId, cell.row.original.id)
                            ),
                            obsId.show
                          )
                        )
                        .mkReactFragment(", ")
                    )
                  )
                  .setEnableSorting(false)
              )
        .useMemoBy((props, _, _) => // rows
          (props.targets.get, props.targetObservations, props.calibrationObservations)
        ): (_, _, _) =>
          (targets, targetObservations, calibrationObservations) =>
            def isCalibrationTarget(targetId: Target.Id): Boolean =
              targetObservations.get(targetId).exists(_.forall(calibrationObservations.contains_))

            targets.toList
              .filterNot((id, _) => isCalibrationTarget(id))
              .map((id, target) => TargetWithId(id, target))
        .useReactTableWithStateStoreBy((props, ctx, cols, rows) =>
          import ctx.given

          def targetIds2RowSelection: List[Target.Id] => RowSelection = targetIds =>
            RowSelection(
              targetIds.map(targetId => RowId(targetId.toString) -> true).toMap
            )

          def rowSelection2TargetIds: RowSelection => List[Target.Id] = selection =>
            selection.value
              .filter(_._2)
              .keys
              .toList
              .map(rowId => Target.Id.parse(rowId.value))
              .flattenOption

          TableOptionsWithStateStore(
            TableOptions(
              cols,
              rows,
              getRowId = (row, _, _) => RowId(row.id.toString),
              enableSorting = true,
              enableColumnResizing = true,
              columnResizeMode = ColumnResizeMode.OnChange,
              enableMultiRowSelection = true,
              state = PartialTableState(
                columnVisibility = props.columnVisibility,
                rowSelection = targetIds2RowSelection(props.selectedTargetIds.get)
              ),
              onRowSelectionChange = (u: Updater[RowSelection]) =>
                (u match
                  case Updater.Set(selection) =>
                    props.selectedTargetIds.set(rowSelection2TargetIds(selection))
                  case Updater.Mod(f)         =>
                    props.selectedTargetIds.mod: targetIds =>
                      rowSelection2TargetIds(f(targetIds2RowSelection(targetIds)))
                ) >> props.focusTargetId(none) // Unselect edited target if rows are selected.
            ),
            TableStore(props.userId, TableId.TargetsSummary, cols)
          )
        )
        .useEffectOnMountBy: (props, _, _, _, table) =>
          props.setToggleAllRowsSelected(table.toggleAllRowsSelected)
        .useRef(none[HTMLTableVirtualizer])
        .useResizeDetector()
        .useEffectWithDepsBy((props, _, _, _, _, _, resizer) => (props.focusedTargetId, resizer)):
          (_, _, _, _, table, virtualizerRef, _) =>
            (focusedTargetId, _) =>
              focusedTargetId.foldMap: targetId =>
                virtualizerRef.get.flatMap: refOpt =>
                  val focusedTargetIdStr: String = targetId.toString
                  Callback:
                    for
                      virtualizer <- refOpt
                      idx         <- table
                                       .getRowModel()
                                       .flatRows
                                       .indexWhere(_.id.value === focusedTargetIdStr)
                                       .some
                                       .filterNot(_ == -1)
                    yield virtualizer.scrollToIndex(idx + 1, ScrollOptions)
        .render: (props, _, _, _, table, virtualizerRef, resizer) =>
          PrimeAutoHeightVirtualizedTable(
            table,
            _ => 32.toPx,
            striped = true,
            compact = Compact.Very,
            innerContainerMod = ^.width := "100%",
            containerRef = resizer.ref,
            tableMod = ExploreStyles.ExploreTable |+| ExploreStyles.ExploreSelectableTable,
            headerCellMod = headerCell =>
              ColumnClasses
                .get(headerCell.column.id)
                .orEmpty |+| ExploreStyles.StickyHeader,
            rowMod = row =>
              TagMod(
                ExploreStyles.TableRowSelected.when_(
                  row.getIsSelected() || props.focusedTargetId.exists(_.toString === row.id.value)
                ),
                ^.onClick ==> table.getMultiRowSelectedHandler(row.id)
              ),
            cellMod = cell => ColumnClasses.get(cell.column.id).orEmpty,
            virtualizerRef = virtualizerRef,
            emptyMessage = <.div("No targets present")
            // workaround to redraw when files are imported
          ).withKey(s"summary-table-${props.filesToImportCount}")

  private case class Title(
    programId:             Program.Id,
    readonly:              Boolean,
    filesToImport:         View[List[DOMFile]],
    columnVisibility:      View[ColumnVisibility],
    toggleAllRowsSelected: Option[Boolean => Callback]
  ) extends ReactFnProps(Title.component)

  private object Title:
    private type Props = Title

    private val component =
      ScalaFnComponent
        .withHooks[Props]
        .useContext(AppContext.ctx)
        .render: (props, ctx) =>
          def onTextChange(e: ReactEventFromInput): Callback =
            val files = e.target.files.toList
            // set value to null so we can reuse the import button
            (Callback(e.target.value = null) *> props.filesToImport.set(files))
              .when_(files.nonEmpty)

          React.Fragment(
            <.div(ExploreStyles.TableSelectionToolbar)(
              React.Fragment(
                if (props.readonly) EmptyVdom
                else
                  HelpIcon("target/main/target-import.md".refined),
                <.label(
                  PrimeStyles.Component |+| PrimeStyles.Button |+| LucumaPrimeStyles.Compact |+| ExploreStyles.FileUpload,
                  ^.htmlFor := "target-import",
                  Icons.FileArrowUp
                ),
                <.input(
                  ^.tpe     := "file",
                  ^.onChange ==> onTextChange,
                  ^.id      := "target-import",
                  ^.name    := "file",
                  ^.accept  := ".csv"
                ),
                TargetImportPopup(props.programId, props.filesToImport),
                props.toggleAllRowsSelected.map: toggleAllRowsSelected =>
                  <.span(
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
                  )
              )
            ),
            ColumnSelectorInTitle(Body.ColNames.toList, props.columnVisibility)
          )
