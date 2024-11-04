// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.Order.given
import explore.Icons
import explore.model.AppContext
import explore.model.ConfigurationRequestList
import explore.model.Observation
import explore.model.TargetList
import explore.model.display.given
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Program
import lucuma.core.syntax.all.*
import lucuma.react.common.ReactFnProps
import lucuma.react.primereact.Tooltip
import lucuma.react.primereact.tooltip.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.react.table.ColumnDef
import lucuma.react.table.ColumnId
import lucuma.ui.LucumaStyles
import lucuma.ui.reusability.given
import lucuma.ui.syntax.table.*
import lucuma.ui.table.*

import scala.collection.immutable.SortedSet

case class ProgramConfigRequestsTile(
  programId:          Program.Id,
  configRequests:     ConfigurationRequestList,
  obs4ConfigRequests: Map[ConfigurationRequest.Id, List[Observation]],
  targets:            TargetList
) extends ReactFnProps(ProgramConfigRequestsTile.component)

object ProgramConfigRequestsTile:

  private type Props = ProgramConfigRequestsTile

  given Reusability[Map[ConfigurationRequest.Id, List[Observation]]] = Reusability.map

  private case class Row(
    request:    ConfigurationRequest,
    obsIds:     SortedSet[Observation.Id],
    targetName: String
  )

  private object Row:
    def apply(
      request:            ConfigurationRequest,
      obs4ConfigRequests: Map[ConfigurationRequest.Id, List[Observation]],
      targets:            TargetList
    ): Row =
      val obses      = obs4ConfigRequests.get(request.id).getOrElse(List.empty)
      val obsIds     = SortedSet.from(obses.map(_.id))
      val targetName = ConfigurationTableColumnBuilder.targetName(obses, targets)
      Row(request, obsIds, targetName)

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
              .obsListColumn(_.obsIds, props.programId, ctx),
            rowColumn(StatusColumnId, _.request.status)
              .setCell(c => stateIcon(c.value))
              .setSize(80.toPx)
              .sortable
          )
    .useMemoBy((props, _, _) => // Rows
      (props.configRequests, props.obs4ConfigRequests, props.targets)
    ): (_, _, _) =>
      (requests, obsMap, targets) => requests.map((_, r) => Row(r, obsMap, targets)).toList
    // TODO: Save state
    .useReactTableBy: (props, _, columns, rows) =>
      TableOptions(
        columns,
        rows,
        getRowId = (row, _, _2) => RowId(row.request.id.toString)
      )
    .useResizeDetector()
    .render { (props, _, _, _, table, resizer) =>
      PrimeAutoHeightVirtualizedTable(
        table,
        _ => 32.toPx,
        striped = true,
        compact = Compact.Very,
        innerContainerMod = ^.width := "100%",
        containerRef = resizer.ref,
        emptyMessage = <.div("There are no requests.")
      )
    }
