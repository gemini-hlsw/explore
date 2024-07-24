// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.syntax.all.*
import explore.components.ui.ExploreStyles
import explore.model.PartnerAllocationList
import explore.model.PartnerAllocations
import explore.model.display.given
import explore.model.reusability.given
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Partner
import lucuma.core.enums.ScienceBand
import lucuma.core.syntax.display.*
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnProps
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.ui.components.TimeSpanView
import lucuma.ui.format.TimeSpanFormatter
import lucuma.ui.table.*

case class TimeAwardTable(allocations: PartnerAllocationList)
    extends ReactFnProps(TimeAwardTable.component)

object TimeAwardTable:
  private type Props = TimeAwardTable

  private case class Row(partner: Partner, allocations: PartnerAllocations):
    lazy val partnerTotal: TimeSpan = allocations.value.values.toList.combineAll

  private object Row:
    def fromPartnerAllocationList(allocations: PartnerAllocationList): List[Row] =
      allocations.value.toList.map(Row(_, _))

  private case class TableMeta(totalByBand: Map[ScienceBand, TimeSpan]):
    lazy val grandTotal: TimeSpan = totalByBand.values.toList.combineAll

  private object TableMeta:
    def fromPartnerAllocationList(allocations: PartnerAllocationList): TableMeta =
      TableMeta(
        totalByBand = Enumerated[ScienceBand].all
          .map: band =>
            band -> allocations.value.values.flatMap(_.value.get(band)).toList.combineAll
          .toMap
      )

  private val ColDef = ColumnDef.WithTableMeta[Row, TableMeta]

  private val PartnerColId: ColumnId                = ColumnId("partner")
  private val BandColId: Map[ScienceBand, ColumnId] =
    Enumerated[ScienceBand].all
      .map: band =>
        band -> ColumnId(band.tag)
      .toMap
  private val TotalColId: ColumnId                  = ColumnId("total")

  private val partnerColDef =
    ColDef(
      PartnerColId,
      _.partner,
      "Time Award",
      cell = cell => <.span(cell.value.abbreviation, cell.value.renderFlag),
      footer = _ => "Total"
    ).setSize(90.toPx)

  private def bandColDef(band: ScienceBand) =
    ColDef(
      BandColId(band),
      _.allocations.value.get(band).orEmpty,
      band.shortName,
      cell = cell => TimeSpanView(cell.value, TimeSpanFormatter.DecimalHours),
      footer = footer =>
        TimeSpanView(
          footer.table.options.meta.foldMap(_.totalByBand(band)),
          TimeSpanFormatter.DecimalHours
        )
    ).setSize(90.toPx)

  private val totalColDef =
    ColDef(
      TotalColId,
      _.partnerTotal,
      "Total",
      cell = cell => TimeSpanView(cell.value, TimeSpanFormatter.DecimalHours),
      footer = footer =>
        TimeSpanView(
          footer.table.options.meta.foldMap(_.grandTotal),
          TimeSpanFormatter.DecimalHours
        )
    ).setSize(90.toPx)

  private val columns: Reusable[List[ColumnDef.WithTableMeta[Row, ?, TableMeta]]] =
    Reusable.always:
      partnerColDef +: Enumerated[ScienceBand].all.map(bandColDef) :+ totalColDef

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useMemoBy(props => props.allocations)(_ => Row.fromPartnerAllocationList)
      .useReactTableBy: (props, rows) =>
        TableOptions(
          columns,
          rows,
          getRowId = (row, _, _) => RowId(row._1.tag),
          meta = TableMeta.fromPartnerAllocationList(props.allocations),
          enableSorting = false,
          enableColumnResizing = false
        )
      .render: (props, _, table) =>
        PrimeTable(
          table,
          tableMod = ExploreStyles.ProgramTabTable
        )
