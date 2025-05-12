// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.syntax.all.*
import explore.components.HelpIcon
import explore.components.ui.ExploreStyles
import explore.model.BandedProgramTime
import explore.model.CategoryAllocationList
import explore.model.ProgramTimes
import explore.model.display.given
import explore.model.reusability.given
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ScienceBand
import lucuma.core.syntax.display.*
import lucuma.core.util.CalculatedValue
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnProps
import lucuma.react.floatingui.syntax.*
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.refined.*
import lucuma.ui.components.TimeSpanView
import lucuma.ui.format.TimeSpanFormatter
import lucuma.ui.reusability.given
import lucuma.ui.table.*

import scala.annotation.targetName

case class TimeAccountingTable(programTimes: ProgramTimes, allocations: CategoryAllocationList)
    extends ReactFnProps(TimeAccountingTable.component)

object TimeAccountingTable:
  given Reusability[Map[ScienceBand, TimeSpan]] = Reusability.map

  private type TimeSpanMap = Map[Option[ScienceBand], CalculatedValue[TimeSpan]]
  private type DataMap     = Map[Option[ScienceBand], Either[CalculatedValue[TimeSpan], BigDecimal]]

  extension (m: TimeSpanMap)
    private def getTimeSpan(osb: Option[ScienceBand]): Option[TimeSpan] =
      m.get(osb).map(_.value)

  extension (e: Either[CalculatedValue[TimeSpan], BigDecimal])
    private def toCell: VdomNode = e match
      case Left(cvts) =>
        TimeSpanView(cvts.value, TimeSpanFormatter.DecimalHours, tooltip = cvts.staleTooltip)
          .withMods(cvts.staleClass)
      case Right(bd)  => <.span(f"${bd * 100}%.1f%%")

  extension (l: List[BandedProgramTime])
    private def toTimeSpanMap: TimeSpanMap =
      l.map(bpt => bpt.band -> bpt.time.value.asReady).toMap

  extension (l: List[CalculatedValue[BandedProgramTime]])
    @targetName("toTimeSpanMapCalculated")
    private def toTimeSpanMap: TimeSpanMap =
      l.map(cvbpt => cvbpt.value.band -> cvbpt.map(_.time.value)).toMap

  private val DataColumnKeys: List[Option[ScienceBand]] =
    Enumerated[ScienceBand].all.map(_.some) :+ none

  private case class Row(
    id:    String,
    label: VdomNode,
    data:  DataMap,
    total: Either[TimeSpan, BigDecimal]
  )

  private object Row:

    private def calcTotal(map: TimeSpanMap): TimeSpan =
      map.values.toList.map(_.value).combineAll

    private def calcPercent(planned: TimeSpan, used: TimeSpan): BigDecimal =
      if (planned.isZero) 0.0
      else used.toMilliseconds / planned.toMilliseconds

    private def fromTimeSpanMap(id: String, map: TimeSpanMap, label: VdomNode): Row =
      val data: DataMap   =
        DataColumnKeys.map(osb => (osb, map.get(osb).orEmpty.asLeft)).toMap
      val total: TimeSpan = calcTotal(map)
      Row(id, label, data, total.asLeft)

    private def completionRow(
      awardedMap: TimeSpanMap,
      usedMap:    TimeSpanMap
    ): Row =
      val data: DataMap     =
        DataColumnKeys
          .map(osb =>
            (osb,
             calcPercent(awardedMap.getTimeSpan(osb).orEmpty,
                         usedMap.getTimeSpan(osb).orEmpty
             ).asRight
            )
          )
          .toMap
      val total: BigDecimal =
        calcPercent(calcTotal(awardedMap), calcTotal(usedMap))
      Row("completion",
          <.span("Completion").withTooltip("Time Used / Time Awarded"),
          data,
          total.asRight
      )

    def remainRow(awardedMap: TimeSpanMap, usedMap: TimeSpanMap): Row =
      val remainMap: Map[Option[ScienceBand], CalculatedValue[TimeSpan]] =
        DataColumnKeys
          .map(osb =>
            (osb, (awardedMap.getTimeSpan(osb).orEmpty -| usedMap.getTimeSpan(osb).orEmpty).asReady)
          )
          .toMap
      fromTimeSpanMap("remain", remainMap, <.span("Remain").withTooltip("Time Awarded - Time Used"))

    def fromProgramTimes(
      awardedMap: TimeSpanMap,
      plannedMap: TimeSpanMap,
      usedMap:    TimeSpanMap
    ): List[Row] =
      val planned: Row    = fromTimeSpanMap("planned", plannedMap, "Prepared")
      val used: Row       = fromTimeSpanMap("used", usedMap, "Used")
      val completion: Row = completionRow(awardedMap, usedMap)
      List(planned, used, completion)

  // A Row is also used for the table metadata for creating the footer
  private val ColDef = ColumnDef[Row].WithTableMeta[Row]

  private val LabelColId: ColumnId = ColumnId("label")
  private val TotalColId: ColumnId = ColumnId("total")

  private val BandColId: Map[Option[ScienceBand], ColumnId] =
    DataColumnKeys.map(osb => (osb, ColumnId(osb.fold("no-band")(_.tag)))).toMap

  private val LabelColumnDef =
    ColDef(
      LabelColId,
      _.label,
      cell = _.value,
      header = _ => <.span("Time Accounting", HelpIcon("program/time-accounting.md".refined)),
      footer = _.table.options.meta.fold(EmptyVdom)(_.label)
    ).withSize(200.toPx)

  private def bandColDef(osb: Option[ScienceBand]) =
    ColDef(
      BandColId(osb),
      _.data(osb),
      header = osb.fold("No Band")(_.shortName),
      cell = _.value.toCell,
      footer = _.table.options.meta.fold(EmptyVdom)(_.data(osb).toCell)
    ).withSize(90.toPx)

  private val TotalColDef =
    ColDef(
      TotalColId,
      _.total,
      "Total",
      cell = _.value.leftMap(_.asReady).toCell,
      footer = _.table.options.meta.fold(EmptyVdom)(_.total.leftMap(_.asReady).toCell)
    ).withSize(90.toPx)

  private val Columns: Reusable[List[ColumnDef.WithTableMeta[Row, ?, Row]]] =
    Reusable.always:
      LabelColumnDef +: DataColumnKeys.map(bandColDef) :+ TotalColDef

  private val component = ScalaFnComponent[TimeAccountingTable]: props =>
    for {
      plannedMap <- useMemo(props.programTimes.timeEstimateBanded)(_.toTimeSpanMap)
      usedMap    <- useMemo(props.programTimes.timeCharge)(_.toTimeSpanMap)
      awardedMap <- useMemo(props.allocations.totalByBand.value.unsorted)(
                      _.map((sb, ts) => sb.some -> ts.asReady).toMap
                    )
      rows       <- useMemo((awardedMap, plannedMap, usedMap)): (awarded, planned, used) =>
                      Row.fromProgramTimes(awarded, planned, used)
      remainRow  <- useMemo((awardedMap, usedMap)): (awarded, used) =>
                      Row.remainRow(awarded, used)
      table      <- useReactTable:
                      TableOptions(
                        Columns,
                        rows,
                        getRowId = (row, _, _) => RowId(row.id),
                        meta = remainRow,
                        enableSorting = false,
                        enableColumnResizing = false
                      )
    } yield PrimeTable(
      table,
      tableMod = ExploreStyles.ProgramTabTable
    )
