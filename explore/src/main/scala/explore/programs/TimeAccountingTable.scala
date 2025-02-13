// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.Monoid
import cats.syntax.all.*
import explore.components.ui.ExploreStyles
import explore.model.BandedProgramTime
import explore.model.ProgramTimes
import explore.model.display.given
import explore.model.reusability.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ScienceBand
import lucuma.core.syntax.display.*
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnProps
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.ui.components.TimeSpanView
import lucuma.ui.format.TimeSpanFormatter
import lucuma.ui.syntax.all.*
import lucuma.ui.table.*

case class TimeAccountingTable(programTimes: ProgramTimes)
    extends ReactFnProps(TimeAccountingTable.component)

object TimeAccountingTable:
  private type TimeSpanMap = Map[Option[ScienceBand], TimeSpan]
  private type DataMap     = Map[Option[ScienceBand], Either[TimeSpan, BigDecimal]]

  extension [K, V: Monoid](map: Map[K, V]) def getM(key: K): V = map.getOrElse(key, Monoid[V].empty)

  extension (e: Either[TimeSpan, BigDecimal])
    def toCell: VdomNode = e match
      case Left(ts)  => TimeSpanView(ts, TimeSpanFormatter.DecimalHours)
      case Right(bd) => f"${bd * 100}%.1f%%"

  extension (l: List[BandedProgramTime])
    def toTimeSpanMap: TimeSpanMap =
      l.map(bpt => bpt.band -> bpt.time.value).toMap

  private val DataColumnKeys: List[Option[ScienceBand]] =
    Enumerated[ScienceBand].all.map(_.some) :+ none

  private case class Row(
    label: String,
    data:  DataMap,
    total: Either[TimeSpan, BigDecimal]
  )

  private object Row:

    private def calcTotal(map: TimeSpanMap): TimeSpan =
      map.values.toList.combineAll

    private def calcPercent(planned: TimeSpan, used: TimeSpan): BigDecimal =
      if (planned.isZero) 0.0
      else used.toMilliseconds / planned.toMilliseconds

    private def fromTimeSpanMap(map: TimeSpanMap, label: String): Row =
      val data: DataMap   =
        DataColumnKeys.map(osb => (osb, map.getM(osb).asLeft)).toMap
      val total: TimeSpan = calcTotal(map)
      Row(label, data, total.asLeft)

    private def completionRow(
      plannedMap: TimeSpanMap,
      usedMap:    TimeSpanMap
    ): Row =
      val data: DataMap     =
        DataColumnKeys
          .map(osb => (osb, calcPercent(plannedMap.getM(osb), usedMap.getM(osb)).asRight))
          .toMap
      val total: BigDecimal =
        calcPercent(calcTotal(plannedMap), calcTotal(usedMap))
      Row("Completion", data, total.asRight)

    def remainRow(plannedMap: TimeSpanMap, usedMap: TimeSpanMap): Row =
      val remainMap: Map[Option[ScienceBand], TimeSpan] =
        DataColumnKeys
          .map(osb => (osb, plannedMap.getM(osb) -| usedMap.getM(osb)))
          .toMap
      fromTimeSpanMap(remainMap, "Remain")

    def fromProgramTimes(plannedMap: TimeSpanMap, usedMap: TimeSpanMap): List[Row] =
      val planned: Row    = fromTimeSpanMap(plannedMap, "Planned")
      val used: Row       = fromTimeSpanMap(usedMap, "Used")
      val completion: Row = completionRow(plannedMap, usedMap)
      List(planned, used, completion)

  // A Row is also used for the table metadata for creating the footer
  private val ColDef = ColumnDef.WithTableMeta[Row, Row]

  private val LabelColId: ColumnId = ColumnId("label")
  private val TotalColId: ColumnId = ColumnId("total")

  private val BandColId: Map[Option[ScienceBand], ColumnId] =
    DataColumnKeys.map(osb => (osb, ColumnId(osb.fold("no-band")(_.tag)))).toMap

  private val LabelColumnDef =
    ColDef(
      LabelColId,
      _.label,
      "Time Accounting",
      footer = _ => "Remain"
    ).setSize(200.toPx)

  private def bandColDef(osb: Option[ScienceBand]) =
    ColDef(
      BandColId(osb),
      _.data(osb),
      header = osb.fold("No Band")(_.shortName),
      cell = _.value.toCell,
      footer = _.table.options.meta.fold(EmptyVdom)(_.data(osb).toCell)
    ).setSize(90.toPx)

  private val TotalColDef =
    ColDef(
      TotalColId,
      _.total,
      "Total",
      cell = _.value.toCell,
      footer = _.table.options.meta.fold(EmptyVdom)(_.total.toCell)
    ).setSize(90.toPx)

  private val Columns: Reusable[List[ColumnDef.WithTableMeta[Row, ?, Row]]] =
    Reusable.always:
      LabelColumnDef +: DataColumnKeys.map(bandColDef) :+ TotalColDef

  private val component = ScalaFnComponent[TimeAccountingTable]: props =>
    for {
      plannedMap <- useMemo(props.programTimes.timeEstimateBanded)(_.toTimeSpanMap)
      usedMap    <- useMemo(props.programTimes.timeCharge)(_.toTimeSpanMap)
      rows       <- useMemo((plannedMap, usedMap)): (planned, used) =>
                      Row.fromProgramTimes(planned, used)
      remainRow  <- useMemo((plannedMap, usedMap)): (planned, used) =>
                      Row.remainRow(planned, used)
      table      <- useReactTable:
                      TableOptions(
                        Columns,
                        rows,
                        getRowId = (row, _, _) => RowId(row.label),
                        meta = remainRow,
                        enableSorting = false,
                        enableColumnResizing = false
                      )
    } yield PrimeTable(
      table,
      tableMod = ExploreStyles.ProgramTabTable
    )
