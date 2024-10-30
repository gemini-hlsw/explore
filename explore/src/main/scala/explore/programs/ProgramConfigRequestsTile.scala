// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.Order.given
import cats.syntax.all.*
import explore.Icons
import explore.model.AppContext
import explore.model.ConfigurationRequestList
import explore.model.Focused
import explore.model.Observation
import explore.model.TargetList
import explore.model.display.given
import explore.model.enums.AppTab
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.math.validation.MathValidators
import lucuma.core.model.Configuration
import lucuma.core.model.Configuration.ObservingMode.GmosNorthLongSlit
import lucuma.core.model.Configuration.ObservingMode.GmosSouthLongSlit
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
      val obses       = obs4ConfigRequests.get(request.id).getOrElse(List.empty)
      val obsIds      = SortedSet.from(obses.map(_.id))
      val targetNames =
        obses
          .flatMap(_.scienceTargetIds)
          .distinct
          .map(tid => targets.get(tid).map(_.name.value))
          .flattenOption
          .distinct
      val targetName  = targetNames match
        case head :: Nil  => head
        case head :: next => "<multiple>"
        case Nil          => "<none>"
      Row(request, obsIds, targetName)

  private val ColDef = ColumnDef[Row]

  private val ConfigRequestIdColumnId = ColumnId("config_request_id")
  private val TargetColumnId          = ColumnId("target")
  private val RAColumnId              = ColumnId("ra")
  private val DecColumnId             = ColumnId("dec")
  private val InstrumentColumnId      = ColumnId("instrument")
  private val FPUColumnId             = ColumnId("fpu")
  private val DisperserColumnId       = ColumnId("disperser")
  private val ImageQualityColumnId    = ColumnId("image_quality")
  private val CloudExtinctionColumnId = ColumnId("cloud_extinction")
  private val SkyBackgroundColumnId   = ColumnId("sky_background")
  private val WaterVaporColumnId      = ColumnId("water_vapor")
  private val ObservationsColumnId    = ColumnId("observations")
  private val StatusColumnId          = ColumnId("status")

  val ColumnNames: Map[ColumnId, String] = Map(
    ConfigRequestIdColumnId -> "ID",
    TargetColumnId          -> "Target",
    RAColumnId              -> "RA",
    DecColumnId             -> "Dec",
    InstrumentColumnId      -> "Instrument",
    FPUColumnId             -> "FPU",
    DisperserColumnId       -> "Disperser",
    ImageQualityColumnId    -> "IQ",
    CloudExtinctionColumnId -> "CC",
    SkyBackgroundColumnId   -> "SB",
    WaterVaporColumnId      -> "WV",
    ObservationsColumnId    -> "Obs",
    StatusColumnId          -> "Status"
  )

  private def configurationColumn[V](
    id:       ColumnId,
    accessor: Configuration => V
  ): ColumnDef.Single.NoMeta[Row, V] =
    ColDef(id, r => accessor(r.request.configuration), ColumnNames(id))

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

  extension (mode: Configuration.ObservingMode)
    def fpu: String       = mode match
      case GmosNorthLongSlit(_) => "LongSlit"
      case GmosSouthLongSlit(_) => "LongSlit"
    def disperser: String = mode match
      case GmosNorthLongSlit(grating) => grating.shortName
      case GmosSouthLongSlit(grating) => grating.shortName

  val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useMemoBy((_, _) => ()): (props, ctx) => // Columns
      _ =>
        def obsUrl(obsId: Observation.Id): String =
          ctx.pageUrl(AppTab.Observations, props.programId, Focused.singleObs(obsId))

        def goToObs(obsId: Observation.Id): Callback =
          ctx.pushPage(AppTab.Observations, props.programId, Focused.singleObs(obsId))

        List(
          rowColumn(ConfigRequestIdColumnId, _.request.id).setSize(90.toPx).sortable,
          rowColumn(TargetColumnId, _.targetName).setSize(150.toPx).sortable,
          configurationColumn(RAColumnId, _.refererenceCoordinates.ra)
            .setCell(c => MathValidators.truncatedRA.reverseGet(c.value))
            .setSize(110.toPx)
            .sortable,
          configurationColumn(DecColumnId, _.refererenceCoordinates.dec)
            .setCell(c => MathValidators.truncatedDec.reverseGet(c.value))
            .setSize(110.toPx)
            .sortable,
          configurationColumn(InstrumentColumnId, _.observingMode.tpe.instrument.shortName)
            .setSize(110.toPx)
            .sortable,
          configurationColumn(FPUColumnId, _.observingMode.fpu).setSize(110.toPx).sortable,
          configurationColumn(DisperserColumnId, _.observingMode.disperser)
            .setSize(110.toPx)
            .sortable,
          configurationColumn(ImageQualityColumnId, _.conditions.imageQuality)
            .setCell(_.value.label)
            .setSize(80.toPx)
            .sortable,
          configurationColumn(CloudExtinctionColumnId, _.conditions.cloudExtinction)
            .setCell(_.value.label)
            .setSize(80.toPx)
            .sortable,
          configurationColumn(SkyBackgroundColumnId, _.conditions.skyBackground)
            .setCell(_.value.label)
            .setSize(80.toPx)
            .sortable,
          configurationColumn(WaterVaporColumnId, _.conditions.waterVapor)
            .setCell(_.value.label)
            .setSize(80.toPx)
            .sortable,
          rowColumn(ObservationsColumnId, _.obsIds)
            .setCell(c =>
              <.span(
                c.value.toList
                  .map(obsId =>
                    <.a(
                      ^.href := obsUrl(obsId),
                      ^.onClick ==> (e =>
                        e.preventDefaultCB >> e.stopPropagationCB >>
                          goToObs(obsId)
                      ),
                      obsId.show
                    )
                  )
                  .mkReactFragment(", ")
              )
            )
            .setSize(150.toPx)
            .setEnableSorting(false),
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
