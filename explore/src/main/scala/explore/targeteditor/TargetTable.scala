// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.Pot
import crystal.react.*
import crystal.react.given
import crystal.react.hooks.*
import explore.Icons
import explore.common.UserPreferencesQueries
import explore.common.UserPreferencesQueries.TableStore
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.AladinFullScreen
import explore.model.AppContext
import explore.model.AsterismIds
import explore.model.ObsIdSet
import explore.model.ObservationsAndTargets
import explore.model.OnAsterismUpdateParams
import explore.model.enums.TableId
import explore.model.extensions.*
import explore.targets.TargetColumns
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Button
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.SiderealTargetWithId
import lucuma.schemas.model.TargetWithId
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*
import lucuma.ui.table.hooks.*

import java.time.Instant

case class TargetTableState(
  table: Option[Table[SiderealTargetWithId, TargetTable.TableMeta]] = None
)

case class TargetTable(
  userId:           Option[User.Id],
  programId:        Program.Id,
  obsIds:           ObsIdSet, // Only used to invoke DB - should only be unexecuted observations
  // Targets are not modified here, we only modify which ones belong to the Asterism.
  targetIds:        AsterismIds,
  obsAndTargets:    UndoSetter[ObservationsAndTargets],
  selectedTarget:   View[Option[Target.Id]],
  onAsterismUpdate: OnAsterismUpdateParams => Callback,
  vizTime:          Option[Instant],
  fullScreen:       AladinFullScreen,
  readOnly:         Boolean
)(val state: View[TargetTableState])
    extends ReactFnProps(TargetTable.component)

object TargetTable extends AsterismModifier:
  private type Props = TargetTable

  case class TableMeta(
    obsIds:           ObsIdSet,
    obsAndTargets:    UndoSetter[ObservationsAndTargets],
    onAsterismUpdate: OnAsterismUpdateParams => Callback
  )

  private val ColDef = ColumnDef.WithTableMeta[SiderealTargetWithId, TableMeta]

  private val DeleteColumnId: ColumnId = ColumnId("delete")

  val columnNames: Map[ColumnId, String] = Map(
    DeleteColumnId -> " "
  ) ++ TargetColumns.AllColNames

  private val columnClasses: Map[ColumnId, Css] = Map(
    DeleteColumnId             -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryDelete),
    TargetColumns.TypeColumnId -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryType),
    TargetColumns.NameColumnId -> (ExploreStyles.StickyColumn |+| ExploreStyles.TargetSummaryName)
  )

  private def deleteSiderealTarget(
    obsIds:           ObsIdSet,
    obsAndTargets:    UndoSetter[ObservationsAndTargets],
    target:           TargetWithId,
    onAsterismUpdate: OnAsterismUpdateParams => Callback
  )(using FetchClient[IO, ObservationDB]): Callback =
    AsterismActions
      .removeTargetFromAsterisms(target, obsIds, onAsterismUpdate)
      .set(obsAndTargets)(true) >>
      // the ".async.toCallback" seems to let the model update before we try changing the UI
      onAsterismUpdate(OnAsterismUpdateParams(target.id, obsIds, false, false)).async.toCallback

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useMemoBy((props, _) => props.readOnly): (_, ctx) => // cols
        readOnly =>
          import ctx.given

          Option
            .unless(readOnly)(
              ColDef(
                DeleteColumnId,
                _.id,
                "",
                cell =>
                  Button(
                    text = true,
                    clazz = ExploreStyles.DeleteButton |+| ExploreStyles.ObsDeleteButton,
                    icon = Icons.Trash,
                    tooltip = "Delete",
                    onClickE = (e: ReactMouseEvent) =>
                      e.preventDefaultCB >>
                        e.stopPropagationCB >>
                        cell.table.options.meta.foldMap(m =>
                          deleteSiderealTarget(
                            m.obsIds,
                            m.obsAndTargets,
                            cell.row.original.toTargetWithId,
                            m.onAsterismUpdate
                          )
                        )
                  ).tiny.compact,
                size = 35.toPx,
                enableSorting = false
              )
            )
            .toList ++
            TargetColumns.Builder.ForProgram(ColDef, _.target.some).AllColumns
      // If vizTime is not set, change it to now
      .useEffectKeepResultWithDepsBy((p, _, _) => p.vizTime): (_, _, _) =>
        vizTime => IO(vizTime.getOrElse(Instant.now()))
      .useMemoBy((props, _, _, vizTime) => (props.targetIds, props.obsAndTargets.get._2, vizTime)): // rows
        (_, _, _, _) =>
          case (targetIds, targetInfo, Pot.Ready(vizTime)) =>
            targetIds.toList
              .map(id =>
                targetInfo
                  .get(id)
                  .flatMap(_.toSiderealAt(vizTime))
                  .map(st => SiderealTargetWithId(id, st))
              )
              .flattenOption
          case _                                           => Nil
      .useReactTableWithStateStoreBy: (props, ctx, cols, _, rows) =>
        import ctx.given

        TableOptionsWithStateStore(
          TableOptions(
            cols,
            rows,
            getRowId = (row, _, _) => RowId(row.id.toString),
            enableSorting = true,
            enableColumnResizing = true,
            columnResizeMode = ColumnResizeMode.OnChange,
            initialState = TableState(columnVisibility = TargetColumns.DefaultVisibility),
            meta = TableMeta(props.obsIds, props.obsAndTargets, props.onAsterismUpdate)
          ),
          TableStore(props.userId, TableId.AsterismTargets, cols)
        )
      .useEffectOnMountBy((p, _, _, _, _, table) => p.state.set(TargetTableState(table.some)))
      .useStateView(AreAdding(false))
      .render: (props, ctx, _, _, rows, table, adding) =>
        import ctx.given

        React.Fragment(
          // props.renderInTitle(
          //   <.span(ExploreStyles.TitleSelectColumns)(
          //     ColumnSelector(table, columnNames, ExploreStyles.SelectColumns)
          //       .unless(props.fullScreen.value)
          //   )
          // ),
          if (rows.isEmpty) {
            <.div(ExploreStyles.HVCenter)(
              targetSelectionPopup(
                "Add a target",
                props.programId,
                props.obsIds,
                props.obsAndTargets,
                adding,
                props.onAsterismUpdate,
                buttonClass = LucumaPrimeStyles.Massive
              )
            )
          } else {
            <.div(ExploreStyles.ExploreTable |+| ExploreStyles.AsterismTable)(
              PrimeTable(
                table,
                striped = true,
                compact = Compact.Very,
                tableMod = ExploreStyles.ExploreTable,
                headerCellMod = headerCell =>
                  columnClasses
                    .get(headerCell.column.id)
                    .orEmpty |+| ExploreStyles.StickyHeader,
                rowMod = row =>
                  TagMod(
                    ExploreStyles.TableRowSelected
                      .when_(props.selectedTarget.get.exists(_ === row.original.id)),
                    ^.onClick --> props.selectedTarget.set(row.original.id.some)
                  ),
                cellMod = cell => columnClasses.get(cell.column.id).orEmpty
              )
            )
          }
        )

case class TargetTableTitle(
  state: View[TargetTableState]
) extends ReactFnProps(TargetTableTitle.component)

object TargetTableTitle:
  private type Props = TargetTableTitle

  private val component =
    ScalaFnComponent[Props]: props =>
      React.Fragment(
        <.span, // Push column selector to right
        <.span(ExploreStyles.TitleSelectColumns)(
          props.state.get.table.map(
            ColumnSelector(_, TargetTable.columnNames, ExploreStyles.SelectColumns)
          )
        )
      )
