// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.programs

import cats.syntax.all.*
import explore.components.ui.ExploreStyles
import explore.model.BandAllocations
import explore.model.CategoryAllocationList
import explore.model.display.given
import explore.model.reusability.given
import explore.syntax.ui.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.syntax.display.*
import lucuma.core.util.Enumerated
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnProps
import lucuma.react.syntax.*
import lucuma.react.table.*
import lucuma.ui.components.TimeSpanView
import lucuma.ui.format.TimeSpanFormatter
import lucuma.ui.table.*

case class TimeAwardTable(allocations: CategoryAllocationList)
    extends ReactFnProps(TimeAwardTable.component)

object TimeAwardTable:
  private case class Row(category: TimeAccountingCategory, allocations: BandAllocations):
    lazy val categoryTotal: TimeSpan = allocations.value.values.toList.combineAll

  private object Row:
    def fromCategoryAllocationList(allocations: CategoryAllocationList): List[Row] =
      allocations.value.toList.map(Row(_, _))

  private case class TableMeta(totalByBand: BandAllocations):
    lazy val grandTotal: TimeSpan = totalByBand.total

  private val ColDef = ColumnDef[Row].WithTableMeta[TableMeta]

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
      _.category,
      "Time Award",
      cell = cell => <.div(cell.value.description, cell.value.renderFlag),
      footer = _ => "Total"
    ).setSize(200.toPx)

  private def bandColDef(band: ScienceBand) =
    ColDef(
      BandColId(band),
      _.allocations.value.get(band).orEmpty,
      band.shortName,
      cell = cell => TimeSpanView(cell.value, TimeSpanFormatter.DecimalHours),
      footer = footer =>
        TimeSpanView(
          footer.table.options.meta.toOption.foldMap(_.totalByBand.value.get(band).orEmpty),
          TimeSpanFormatter.DecimalHours
        )
    ).setSize(90.toPx)

  private val totalColDef =
    ColDef(
      TotalColId,
      _.categoryTotal,
      "Total",
      cell = cell => TimeSpanView(cell.value, TimeSpanFormatter.DecimalHours),
      footer = footer =>
        TimeSpanView(
          footer.table.options.meta.toOption.foldMap(_.grandTotal),
          TimeSpanFormatter.DecimalHours
        )
    ).setSize(90.toPx)

  private val columns: Reusable[List[ColumnDef.WithTableMeta[Row, ?, TableMeta]]] =
    Reusable.always:
      partnerColDef +: Enumerated[ScienceBand].all.map(bandColDef) :+ totalColDef

  private val component =
    ScalaFnComponent[TimeAwardTable]: props =>
      for {
        rows  <- useMemo(props.allocations)(Row.fromCategoryAllocationList)
        table <- useReactTable:
                   TableOptions(
                     columns,
                     rows,
                     getRowId = (row, _, _) => RowId(row._1.tag),
                     meta = TableMeta(props.allocations.totalByBand),
                     enableSorting = false,
                     enableColumnResizing = false
                   )
      } yield PrimeTable(
        table,
        tableMod = ExploreStyles.ProgramTabTable
      )
