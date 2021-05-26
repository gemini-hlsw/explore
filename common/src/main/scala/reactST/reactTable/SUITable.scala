// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package reactST.reactTable

import cats.syntax.all._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.JsFn
import japgolly.scalajs.react.internal.Box
import japgolly.scalajs.react.vdom.html_<^._
import react.semanticui.collections.table._
import reactST.reactTable._
import reactST.reactTable.mod.{ ^ => _, _ }
import reactST.reactTable.syntax._
import reactST.reactTable.util

import scalajs.js
import scalajs.js.|

case class SUITableProps[D, TableInstanceD <: TableInstance[D]](
  table:        Table = Table(),
  header:       Boolean | TableHeader = false,
  headerRow:    TableRow = TableRow(),
  headerCell:   TableHeaderCell = TableHeaderCell(),
  body:         TableBody = TableBody(),
  row:          TableRow = TableRow(),
  cell:         TableCell = TableCell(),
  footer:       Boolean | TableFooter | VdomNode = false,
  footerRow:    TableRow = TableRow(),
  footerCell:   TableHeaderCell = TableHeaderCell()
)(val instance: TableInstanceD)

class SUITable[
  D,
  TableOptsD <: UseTableOptions[D],
  TableInstanceD <: TableInstance[D],
  ColumnOptsD <: ColumnOptions[D],
  ColumnInstanceD <: ColumnObject[D],
  State <: TableState[D] // format: on
](
  tableMaker: TableMaker[D, TableOptsD, TableInstanceD, ColumnOptsD, ColumnInstanceD, State]
) {
  private implicit def props2Attrs(obj: js.Object): TagMod = util.props2Attrs(obj)

  val component = ScalaFnComponent[SUITableProps[D, TableInstanceD]] { case props =>
    val tableInstance = props.instance

    val headerTag: Option[TableHeader] = (props.header: Any) match {
      case true               => TableHeader().some
      case false              => none
      case other: TableHeader => other.some
      case _                  => ??? // Can't wait for Scala 3's union types
    }

    val headerElement: Option[TableHeader] =
      headerTag.map(_(tableInstance.headerGroups.toTagMod { headerRowData =>
        props.headerRow(headerRowData.getHeaderGroupProps())(
          TableMaker
            .headersFromGroup(headerRowData)
            .toTagMod { headerCellData: HeaderGroup[D] =>
              props.headerCell(headerCellData.getHeaderProps())(
                // headerCellData.renderHeader
                headerCellData.render_Header(reactTableStrings.Header)
              )
            }
        )
      }))

    val bodyElement: TableBody = props.body(tableInstance.getTableBodyProps())(
      tableInstance.rows.toTagMod { rowData =>
        tableInstance.prepareRow(rowData)
        props.row(rowData.getRowProps())(rowData.cells.toTagMod { cellData =>
          props.cell(cellData.getCellProps())(cellData.renderCell)
        })
      }
    )

    def standardFooter(footerTag: TableFooter) =
      footerTag(tableInstance.footerGroups.toTagMod { footerRowData =>
        props.footerRow(footerRowData.getFooterGroupProps())(
          TableMaker.headersFromGroup(footerRowData).toTagMod { footerCellData: HeaderGroup[D] =>
            props.footerCell(footerCellData.getFooterProps())(
              // footerCellData.renderFooter
              footerCellData.render_Footer(reactTableStrings.Footer)
            )
          }
        )
      })

    val footerElement: Option[VdomNode] = (props.footer: Any) match {
      case true                     => standardFooter(TableFooter()).vdomElement.some
      case false                    => none
      case otherFooter: TableFooter => standardFooter(otherFooter).vdomElement.some
      case otherElement: VdomNode   => otherElement.some
      case _                        => ??? // Can't wait for Scala 3's union types
    }

    props
      .table(
        headerElement,
        bodyElement,
        footerElement
      )
      .vdomElement
  }

  private type Props = SUITableProps[D, TableInstanceD]

  private type Component = JsFn.UnmountedWithRoot[Props, Unit, Box[Props]]

  protected case class Applied(builder: TableInstanceD => Component) {
    def apply(instance: TableInstanceD): Component = builder(instance)

    def apply(options: TableOptsD): Component = builder(tableMaker.use(options))

    def apply(columns: js.Array[_ <: UseTableColumnOptions[D]], data: js.Array[D]): Component =
      apply(tableMaker.Options(columns, data))
  }

  def apply(
    table:      Table = Table(),
    header:     Boolean | TableHeader = false,
    headerRow:  TableRow = TableRow(),
    headerCell: TableHeaderCell = TableHeaderCell(),
    body:       TableBody = TableBody(),
    row:        TableRow = TableRow(),
    cell:       TableCell = TableCell(),
    footer:     Boolean | TableFooter | VdomNode = false,
    footerRow:  TableRow = TableRow(),
    footerCell: TableHeaderCell = TableHeaderCell()
  ): Applied = Applied((instance: TableInstanceD) =>
    component(
      SUITableProps(table,
                    header,
                    headerRow,
                    headerCell,
                    body,
                    row,
                    cell,
                    footer,
                    footerRow,
                    footerCell
      )(instance)
    )
  )
}
