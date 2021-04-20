package explore.config

import reactST.reactTable._
import reactST.reactTable.mod.{ ^ => _, _ }
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import scalajs.js
import react.semanticui.collections.table._

case class SUITableMaker[D](
  table:       Table = Table(),
  header:      js.UndefOr[TableHeader] = js.undefined,
  headerRow:   TableRow = TableRow(),
  headerCell:  TableHeaderCell = TableHeaderCell(),
  body:        TableBody = TableBody(),
  row:         TableRow = TableRow(),
  cell:        TableCell = TableCell(),
  footer:      js.UndefOr[TableFooter] = js.undefined,
  footerRow:   TableRow = TableRow(),
  footerCell:  TableHeaderCell = TableHeaderCell(),
  val plugins: List[PluginHook[D]] = List.empty
) extends // format: off
  TableMaker2[D, UseTableOptions[D], TableInstance[D], ColumnWithLooseAccessor[D], 
    ColumnInstance[D], TableState[D]] { self => // format: on

  private implicit def props2Attrs(obj: js.Object): TagMod = TableMaker.props2Attrs(obj)

  val component =
    ScalaFnComponent[(UseTableOptions[D], js.Array[D])] { case (opts, data) =>
      val tableInstance = use(opts.setData(data))

      table(
        header.map(_(tableInstance.headerGroups.toTagMod { headerRowData =>
          headerRow(headerRowData.getHeaderGroupProps())(
            headersFromGroup(headerRowData).toTagMod(headerCellData =>
              headerCell(headerCellData.getHeaderProps())(
                headerCellData.render("Header")
              )
            )
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
            headersFromGroup(footerRowData).toTagMod(footerCellData =>
              footerCell(footerCellData.getFooterProps())(
                footerCellData.render("Footer")
              )
            )
          )
        }))
      ).vdomElement
    }
}
