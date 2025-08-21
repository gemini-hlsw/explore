// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.itc

import cats.Eq
import cats.data.EitherNec
import cats.derived.*
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.config.ModesTableCommon
import explore.model.ObsTabTileIds
import explore.model.Observation
import explore.model.Progress
import explore.model.TargetList
import explore.model.itc.*
import explore.model.reusability.given
import explore.modes.ConfigSelection
import explore.modes.ItcInstrumentConfig
import explore.modes.ModeRow
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.User
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.itc.SignalToNoiseAt
import lucuma.itc.SingleSN
import lucuma.itc.TotalSN
import lucuma.react.common.ReactFnComponent
import lucuma.react.common.ReactFnProps
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.ui.syntax.all.given
import lucuma.ui.table.*

object ItcImagingTile extends ModesTableCommon:
  case class ImagingFilterRow(
    id:         Int,
    instrument: ItcInstrumentConfig,
    result:     Pot[EitherNec[ItcTargetProblem, ItcResult]]
  ) extends ModeRow
      with TableRowWithResult derives Eq:

    val config  = instrument
    def enabled = true

    private def withResult[A](f: (TimeSpan, PosInt, Option[SignalToNoiseAt]) => A): Option[A] =
      result.toOption.collect { case Right(ItcResult.Result(e, t, _, s)) => f(e, t, s) }

    val singleSN: Option[SingleSN] =
      withResult((_, _, s) => s.map(_.single)).flatten

    override lazy val totalSN: Option[TotalSN] =
      withResult((_, _, s) => s.map(_.total)).flatten

    val exposureTime: Option[TimeSpan] =
      withResult((e, _, _) => e)

    val exposureCount: Option[PosInt] =
      withResult((_, t, _) => t)

  def apply(
    uid:                 Option[User.Id],
    selectedConfigs:     ConfigSelection,
    observation:         Observation,
    obsTargets:          TargetList,
    customSedTimestamps: List[Timestamp]
  ) =
    Tile(
      ObsTabTileIds.ItcId.id,
      s"ITC",
      ItcTileState.Empty,
      bodyClass = ExploreStyles.ItcImagingTileBody
    )(
      s =>
        uid.map(
          Body(
            _,
            selectedConfigs,
            observation,
            obsTargets,
            customSedTimestamps,
            s
          )
        ),
      (s, _) =>
        Title(
          observation,
          selectedConfigs,
          obsTargets,
          customSedTimestamps,
          s
        )
    )

  private val ColDef = ColumnDef[ImagingFilterRow].WithTableMeta[TableMeta]

  private val FilterColId     = ColumnId("filter")
  private val InstrumentColId = ColumnId("instrument")
  private val TotalSNColId    = ColumnId("totalsn")
  private val ExpTimeColId    = ColumnId("exptime")
  private val ExposuresColId  = ColumnId("exposures")

  private val columnNames: Map[ColumnId, String] =
    Map(
      InstrumentColId -> "Instrument",
      ExpTimeColId    -> "Time",
      TotalSNColId    -> "S/N",
      FilterColId     -> "Filter",
      ExposuresColId  -> "Exposures"
    )

  private def column[V](
    id:       ColumnId,
    accessor: ImagingFilterRow => V
  ): ColumnDef.Single.WithTableMeta[ImagingFilterRow, V, TableMeta] =
    ColDef(id, accessor, columnNames.getOrElse(id, id.value))

  private lazy val columns =
    List(
      column(InstrumentColId, _.config.instrument.shortName)
        .withCell(_.value: String)
        .withSize(120.toPx),
      column(FilterColId, _.config.filterStr)
        .withCell(_.value: String)
        .withSize(69.toPx)
        .sortable,
      column(ExposuresColId, _.result)
        .withHeader(progressingCellHeader("Exposures"))
        .withCell: cell =>
          itcCell(cell.value, ItcColumns.Exposures)
        .withSize(80.toPx),
      column(ExpTimeColId, _.result)
        .withHeader(progressingCellHeader("Time"))
        .withCell: cell =>
          itcCell(cell.value, ItcColumns.Time)
        .withSize(85.toPx),
      column(TotalSNColId, _.result)
        .withHeader(progressingCellHeader("S/N"))
        .withCell: cell =>
          itcCell(cell.value, ItcColumns.SN)
        .withSize(85.toPx)
    )

  private case class Body(
    uid:                 User.Id,
    selectedConfigs:     ConfigSelection,
    observation:         Observation,
    obsTargets:          TargetList,
    customSedTimestamps: List[Timestamp],
    tileState:           View[ItcTileState]
  ) extends ReactFnProps(Body)

  private object Body
      extends ReactFnComponent[Body](props =>
        for {
          rows  <- useMemo(props.selectedConfigs): selection =>
                     selection.configs.zipWithIndex.map { (configAndResult, id) =>
                       val config = configAndResult.instrumentConfig
                       val result = Pot.fromOption(configAndResult.itcResult)
                       ImagingFilterRow(id, config, result)
                     }
          cols  <- useMemo(()): _ =>
                     columns
          table <- useReactTable(
                     TableOptions(
                       cols,
                       rows,
                       getRowId = (row, _, _) => RowId(row.id.toString),
                       enableSorting = true,
                       enableColumnResizing = true,
                       meta = TableMeta(none[Progress])
                     )
                   )
        } yield <.div(
          ExploreStyles.ItcTileBody,
          if (rows.nonEmpty)
            PrimeTable(table, compact = Compact.Very)
          else
            <.p("No configurations selected")
        )
      )

  private case class Title(
    observation:         Observation,
    selectedConfigs:     ConfigSelection,
    obsTargets:          TargetList,
    customSedTimestamps: List[Timestamp],
    tileState:           View[ItcTileState]
  ) extends ReactFnProps(Title)

  given Reusability[ImagingTargetAndResults] = Reusability.byEq

  private object Title
      extends ReactFnComponent[Title](_ =>
        // TODO: Add target selector
        EmptyVdom
      )
