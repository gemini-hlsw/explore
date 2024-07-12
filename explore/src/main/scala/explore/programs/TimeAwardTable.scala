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
import lucuma.ui.TimeUnitsFormat
import lucuma.ui.components.TimeSpanView
import lucuma.ui.table.*

case class TimeAwardTable(allocations: PartnerAllocationList)
    extends ReactFnProps(TimeAwardTable.component)

object TimeAwardTable:
  private type Props = TimeAwardTable

  private val ColDef = ColumnDef[(Partner, PartnerAllocations)]

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
      _._1,
      "Time Award",
      cell = cell => <.span(cell.value.abbreviation, cell.value.renderFlag),
      footer = _ => "Total"
    ).setSize(90.toPx)

  private def bandColDef(band: ScienceBand) =
    ColDef(
      BandColId(band),
      _._2.value.get(band).getOrElse(TimeSpan.Zero),
      band.shortName,
      cell = cell => TimeSpanView(cell.value, TimeUnitsFormat.Letter),
      footer = footer =>
        val total = footer.table
          .getRowModel()
          .rows
          .map(_.original._2.value.get(band).getOrElse(TimeSpan.Zero))
          .combineAll
        TimeSpanView(total, TimeUnitsFormat.Letter)
    ).setSize(90.toPx)

  private val totalColDef =
    ColDef(
      TotalColId,
      _._2.value.values.toList.combineAll,
      "Total",
      cell = cell => TimeSpanView(cell.value, TimeUnitsFormat.Letter),
      footer = footer =>
        val total = footer.table
          .getRowModel()
          .rows
          .map(_.original._2.value.values.toList.combineAll)
          .combineAll
        TimeSpanView(total, TimeUnitsFormat.Letter)
    ).setSize(90.toPx)

  private val columns: Reusable[List[ColumnDef.NoMeta[(Partner, PartnerAllocations), ?]]] =
    Reusable.always:
      partnerColDef +: Enumerated[ScienceBand].all.map(bandColDef) :+ totalColDef

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useMemoBy(props => props.allocations)(_ => _.value.toList)
      .useReactTableBy: (_, rows) =>
        TableOptions(
          columns,
          rows,
          getRowId = (row, _, _) => RowId(row._1.tag),
          enableSorting = false,
          enableColumnResizing = false
        )
      .render: (props, _, table) =>
        PrimeTable(
          table,
          tableMod = ExploreStyles.ProgramTabTable
        )
