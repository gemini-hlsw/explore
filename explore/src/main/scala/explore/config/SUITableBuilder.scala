// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.semanticui.collections.table._
import reactST.reactTable._
import reactST.reactTable.mod.{ ^ => _, _ }

import scalajs.js

object SUITableBuilder {
  private implicit def props2Attrs(obj: js.Object): TagMod = TableMaker.props2Attrs(obj)

  def buildComponent[
    D,
    TableOptsD <: UseTableOptions[D],
    TableInstanceD <: TableInstance[D],
    ColumnOptsD <: ColumnWithLooseAccessor[D],
    ColumnInstanceD <: ColumnInstance[D],
    State <: TableState[D] // format: on
  ](
    tableMaker: TableMaker[D, TableOptsD, TableInstanceD, ColumnOptsD, ColumnInstanceD, State],
    table:      Table = Table(),
    header:     js.UndefOr[TableHeader] = js.undefined,
    headerRow:  TableRow = TableRow(),
    headerCell: TableHeaderCell = TableHeaderCell(),
    body:       TableBody = TableBody(),
    row:        TableRow = TableRow(),
    cell:       TableCell = TableCell(),
    footer:     js.UndefOr[TableFooter] = js.undefined,
    footerRow:  TableRow = TableRow(),
    footerCell: TableHeaderCell = TableHeaderCell()
  ) =
    ScalaFnComponent[(TableOptsD, js.Array[D])] { case (opts, data) =>
      val tableInstance = tableMaker.use(opts.setData(data))

      table(
        header.map(_(tableInstance.headerGroups.toTagMod { headerRowData =>
          headerRow(headerRowData.getHeaderGroupProps())(
            TableMaker
              .headersFromGroup(headerRowData)
              .toTagMod { headerCellData: HeaderGroup[D] =>
                headerCell(headerCellData.getHeaderProps())(
                  headerCellData.render("Header")
                )
              }
          )
        })),
        body(tableInstance.getTableBodyProps())(
          tableInstance.rows.toTagMod { rowData =>
            tableInstance.prepareRow(rowData)
            row(rowData.getRowProps())(rowData.cells.toTagMod { cellData =>
              cell(cellData.getCellProps())(cellData.render("Cell"))
            })
          }
        ),
        footer.map(_(tableInstance.footerGroups.toTagMod { footerRowData =>
          footerRow(footerRowData.getFooterGroupProps())(
            TableMaker.headersFromGroup(footerRowData).toTagMod { footerCellData: HeaderGroup[D] =>
              footerCell(footerCellData.getFooterProps())(
                footerCellData.render("Footer")
              )
            }
          )
        }))
      ).vdomElement
    }
}
