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

// We can't define a package object since it's already defined in the facade.
object definitions {
  type HeaderCellRender[D, ColumnInstanceD <: ColumnObject[D]] = ColumnInstanceD => TableHeaderCell
  type HeaderCell[D, ColumnInstanceD <: ColumnObject[D]]       =
    TableHeaderCell | HeaderCellRender[D, ColumnInstanceD]

  type BodyCellRender[D] = Cell[D, _] => TableCell
  type BodyCell[D]       = TableCell | BodyCellRender[D]

  protected[reactTable] def defaultHeaderCell[D, ColumnInstanceD <: ColumnObject[D]]
    : HeaderCell[D, ColumnInstanceD] = TableHeaderCell()

  protected[reactTable] def defaultBodyCell[D]: BodyCell[D] = TableCell()
}
import definitions._

case class SUITableProps[D, TableInstanceD <: TableInstance[D], ColumnInstanceD <: ColumnObject[D]](
  table:        Table = Table(),
  header:       Boolean | TableHeader = false,
  headerRow:    TableRow = TableRow(),
  headerCell:   HeaderCell[D, ColumnInstanceD] = defaultHeaderCell[D, ColumnInstanceD],
  body:         TableBody = TableBody(),
  row:          TableRow = TableRow(),
  cell:         BodyCell[D] = defaultBodyCell[D],
  footer:       Boolean | TableFooter | VdomNode = false,
  footerRow:    TableRow = TableRow(),
  footerCell:   HeaderCell[D, ColumnInstanceD] = defaultHeaderCell[D, ColumnInstanceD]
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

  val component = ScalaFnComponent[SUITableProps[D, TableInstanceD, ColumnInstanceD]] {
    case props =>
      val tableInstance = props.instance

      val headerTag: Option[TableHeader] = (props.header: Any) match {
        case true  => TableHeader().some
        case false => none
        case other => other.asInstanceOf[TableHeader].some // Can't wait for Scala 3's union types
      }

      val headerCellRender: HeaderCellRender[D, ColumnInstanceD] = (props.headerCell: Any) match {
        case headerCell: TableHeaderCell =>
          col => headerCell(col.getHeaderProps())(col.renderHeader)
        case other                       => other.asInstanceOf[HeaderCellRender[D, ColumnInstanceD]]
      }

      val bodyCellRender: BodyCellRender[D] = (props.cell: Any) match {
        case cell: TableCell =>
          cellData => cell(cellData.getCellProps())(cellData.renderCell)
        case other           => other.asInstanceOf[BodyCellRender[D]]
      }

      val footerCellRender: HeaderCellRender[D, ColumnInstanceD] = (props.footerCell: Any) match {
        case footerCell: TableHeaderCell =>
          col => footerCell(col.getFooterProps())(col.renderFooter)
        case other                       => other.asInstanceOf[HeaderCellRender[D, ColumnInstanceD]]
      }

      val headerElement: Option[TableHeader] =
        headerTag.map(_(tableInstance.headerGroups.toTagMod { headerRowData =>
          props.headerRow(headerRowData.getHeaderGroupProps())(
            TableMaker
              .headersFromGroup(headerRowData)
              .toTagMod((col: ColumnInstanceD) => headerCellRender(col))
          )
        }))

      val bodyElement: TableBody = props.body(tableInstance.getTableBodyProps())(
        tableInstance.rows.toTagMod { rowData =>
          tableInstance.prepareRow(rowData)
          props.row(rowData.getRowProps())(
            rowData.cells.toTagMod(cellData => bodyCellRender(cellData))
          )
        }
      )

      def standardFooter(footerTag: TableFooter) =
        footerTag(tableInstance.footerGroups.toTagMod { footerRowData =>
          props.footerRow(footerRowData.getFooterGroupProps())(
            TableMaker
              .headersFromGroup(footerRowData)
              .toTagMod((col: ColumnInstanceD) => footerCellRender(col))
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

  private type Props = SUITableProps[D, TableInstanceD, ColumnInstanceD]

  private type Component = JsFn.UnmountedWithRoot[Props, Unit, Box[Props]]

  case class Applied(builder: TableInstanceD => Component) {
    def apply(instance: TableInstanceD): Component = builder(instance)

    def apply(options: TableOptsD): Component = builder(tableMaker.use(options))

    def apply(columns: js.Array[_ <: UseTableColumnOptions[D]], data: js.Array[D]): Component =
      apply(tableMaker.Options(columns, data))
  }

  def apply(
    table:      Table = Table(),
    header:     Boolean | TableHeader = false,
    headerRow:  TableRow = TableRow(),
    headerCell: HeaderCell[D, ColumnInstanceD] = defaultHeaderCell[D, ColumnInstanceD],
    body:       TableBody = TableBody(),
    row:        TableRow = TableRow(),
    cell:       BodyCell[D] = defaultBodyCell[D],
    footer:     Boolean | TableFooter | VdomNode = false,
    footerRow:  TableRow = TableRow(),
    footerCell: HeaderCell[D, ColumnInstanceD] = defaultHeaderCell[D, ColumnInstanceD]
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
