// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package reactST.reactTable

import cats.syntax.all._
import explore.Icons
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.JsFn
import japgolly.scalajs.react.internal.Box
import japgolly.scalajs.react.vdom.html_<^._
import react.semanticui.AsC
import react.semanticui.collections.table._
import reactST.reactTable._
import reactST.reactTable.mod.{ ^ => _, _ }
import reactST.reactTable.syntax._
import reactST.reactTable.util

import scalajs.js
import scalajs.js.|

// We can't define a package object since it's already defined in the facade.
object definitions {
  type TableRender[D, TableInstanceD <: TableInstance[D]]   = TableInstanceD => Table
  type TableTemplate[D, TableInstanceD <: TableInstance[D]] =
    Table | TableRender[D, TableInstanceD]

  type HeaderCellRender[D, ColumnInstanceD <: ColumnObject[D]] = ColumnInstanceD => TableHeaderCell
  type HeaderCell[D, ColumnInstanceD <: ColumnObject[D]]       =
    TableHeaderCell | HeaderCellRender[D, ColumnInstanceD]

  type BodyCellRender[D] = Cell[D, _] => TableCell
  type BodyCell[D]       = TableCell | BodyCellRender[D]

  implicit def props2Attrs(obj: js.Object): TagMod = util.props2Attrs(obj)
}
import definitions._

trait LayoutDefaultTag[Layout] {
  val tag: js.UndefOr[AsC]
}
object LayoutDefaultTag        {
  implicit object TableLayoutDefaultTag extends LayoutDefaultTag[Layout.Table] {
    val tag = js.undefined
  }

  implicit object NonTableLayoutDefaultTag extends LayoutDefaultTag[Layout.NonTable] {
    val tag = <.div
  }
}

trait SortElements[Col]       {
  val props: Col => TagMod
  val indicator: Col => TagMod
}
trait LowPrioritySortElements {
  implicit def unsortableColElements[C]: SortElements[C] =
    new SortElements[C] {
      val props     = _ => TagMod.empty
      val indicator = _ => TagMod.empty
    }
}
object SortElements extends LowPrioritySortElements {
  val sortDown = Icons.SortDown.fixedWidth()
  val sortUp   = Icons.SortUp.fixedWidth()
  val sort     = Icons.Sort.fixedWidth()

  implicit def sortableColElements[C <: UseSortByColumnProps[_]]: SortElements[C] =
    new SortElements[C] {
      val props     = _.getSortByToggleProps()
      val indicator = col =>
        if (col.isSorted) {
          val index             = if (col.sortedIndex > 0) s"${col.sortedIndex + 1}" else ""
          val ascDesc: VdomNode =
            if (col.isSortedDesc.getOrElse(false)) sortDown else sortUp
          <.span(ascDesc, <.small(index))
        } else sort.unless(!col.canSort)
    }
}

protected case class SUITableProps[D, TableInstanceD <: TableInstance[
  D
], ColumnInstanceD <: ColumnObject[D]](
  table:        TableTemplate[D, TableInstanceD],
  header:       Boolean | TableHeader,
  headerRow:    TableRow = TableRow(),
  headerCell:   HeaderCell[D, ColumnInstanceD],
  body:         TableBody,
  row:          TableRow,
  cell:         BodyCell[D],
  footer:       Boolean | TableFooter | VdomNode,
  footerRow:    TableRow,
  footerCell:   HeaderCell[D, ColumnInstanceD]
)(val instance: TableInstanceD)

class SUITable[
  D,
  TableOptsD <: UseTableOptions[D],
  TableInstanceD <: TableInstance[D],
  ColumnOptsD <: ColumnOptions[D],
  ColumnInstanceD <: ColumnObject[D],
  State <: TableState[D],
  Layout // format: on
](
  tableMaker:   TableMaker[D, TableOptsD, TableInstanceD, ColumnOptsD, ColumnInstanceD, State, Layout]
)(implicit
  layout:       LayoutDefaultTag[Layout],
  sortElements: SortElements[ColumnInstanceD]
) {
  val component = ScalaFnComponent[SUITableProps[D, TableInstanceD, ColumnInstanceD]] {
    case props =>
      val tableInstance = props.instance

      val tableRender: TableRender[D, TableInstanceD] = (props.table: Any) match {
        case table: Table => tableInstance => table(tableInstance.getTableProps())
        case other        => other.asInstanceOf[TableRender[D, TableInstanceD]]
      }

      val headerTag: Option[TableHeader] = (props.header: Any) match {
        case true  => TableHeader(as = layout.tag).some
        case false => none
        case other => other.asInstanceOf[TableHeader].some // Can't wait for Scala 3's union types
      }

      val headerCellRender: HeaderCellRender[D, ColumnInstanceD] =
        (props.headerCell: Any) match {
          case headerCell: TableHeaderCell => _ => headerCell
          case fn                          => fn.asInstanceOf[HeaderCellRender[D, ColumnInstanceD]]
        }

      val bodyCellRender: BodyCellRender[D] = (props.cell: Any) match {
        case cell: TableCell => _ => cell
        case fn              => fn.asInstanceOf[BodyCellRender[D]]
      }

      val footerCellRender: HeaderCellRender[D, ColumnInstanceD] = (props.footerCell: Any) match {
        case footerCell: TableHeaderCell => _ => footerCell
        case fn                          => fn.asInstanceOf[HeaderCellRender[D, ColumnInstanceD]]
      }

      val headerElement: Option[TableHeader] =
        headerTag.map(_(tableInstance.headerGroups.toTagMod { headerRowData =>
          props.headerRow(headerRowData.getHeaderGroupProps())(
            TableMaker
              .headersFromGroup(headerRowData)
              .toTagMod((col: ColumnInstanceD) =>
                headerCellRender(col)(col.getHeaderProps(), sortElements.props(col))(
                  col.renderHeader,
                  sortElements.indicator(col)
                )
              )
          )
        }))

      val bodyElement: TableBody = props.body(tableInstance.getTableBodyProps())(
        tableInstance.rows.toTagMod { rowData =>
          tableInstance.prepareRow(rowData)
          props.row(rowData.getRowProps())(
            rowData.cells.toTagMod(cellData =>
              bodyCellRender(cellData)(cellData.getCellProps())(cellData.renderCell)
            )
          )
        }
      )

      def standardFooter(footerTag: TableFooter) =
        footerTag(tableInstance.footerGroups.toTagMod { footerRowData =>
          props.footerRow(footerRowData.getFooterGroupProps())(
            TableMaker
              .headersFromGroup(footerRowData)
              .toTagMod((col: ColumnInstanceD) =>
                footerCellRender(col)(col.getFooterProps())(col.renderFooter)
              )
          )
        })

      val footerElement: Option[VdomNode] = (props.footer: Any) match {
        case true                     => standardFooter(TableFooter()).vdomElement.some
        case false                    => none
        case otherFooter: TableFooter => standardFooter(otherFooter).vdomElement.some
        case otherElement: VdomNode   => otherElement.some
        case _                        => ??? // Can't wait for Scala 3's union types
      }

      tableRender(tableInstance)(
        headerElement,
        bodyElement,
        footerElement
      ).vdomElement
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
    table:      TableTemplate[D, TableInstanceD] = Table(as = layout.tag),
    header:     Boolean | TableHeader = false,
    headerRow:  TableRow = TableRow(as = layout.tag, cellAs = layout.tag),
    headerCell: HeaderCell[D, ColumnInstanceD] = TableHeaderCell(as = layout.tag),
    body:       TableBody = TableBody(as = layout.tag),
    row:        TableRow = TableRow(as = layout.tag, cellAs = layout.tag),
    cell:       BodyCell[D] = TableCell(as = layout.tag),
    footer:     Boolean | TableFooter | VdomNode = false,
    footerRow:  TableRow = TableRow(as = layout.tag, cellAs = layout.tag),
    footerCell: HeaderCell[D, ColumnInstanceD] = TableHeaderCell(as = layout.tag)
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
