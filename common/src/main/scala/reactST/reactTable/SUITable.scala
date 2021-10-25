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
import reactST.reactTable.syntax._
import reactST.reactTable.util

import scalajs.js
import scalajs.js.|
import reactST.reactTable.facade.tableInstance.TableInstance
import reactST.reactTable.facade.column.Column
import reactST.reactTable.facade.row.Row
import reactST.reactTable.facade.cell.Cell

// We can't define a package object since it's already defined in the facade.
object definitions {
  type TableRender[D, Plugins]   = TableInstance[D, Plugins] => Table
  type TableTemplate[D, Plugins] = Table | TableRender[D, Plugins]

  type HeaderCellRender[D, Plugins] = Column[D, Plugins] => TableHeaderCell
  type HeaderCell[D, Plugins]       = TableHeaderCell | HeaderCellRender[D, Plugins]

  type RowRender[D, Plugins]   = Row[D, Plugins] => TableRow
  type RowTemplate[D, Plugins] = TableRow | RowRender[D, Plugins]

  type BodyCellRender[D, Plugins] = Cell[D, _, Plugins] => TableCell
  type BodyCell[D, Plugins]       = TableCell | BodyCellRender[D, Plugins]

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

trait SortElements[D, Plugins] {
  val props: Column[D, Plugins] => TagMod
  val indicator: Column[D, Plugins] => TagMod
}
trait LowPrioritySortElements  {
  implicit def unsortableColElements[D, Plugins]: SortElements[D, Plugins] =
    new SortElements[D, Plugins] {
      val props     = _ => TagMod.empty
      val indicator = _ => TagMod.empty
    }
}
object SortElements extends LowPrioritySortElements {
  val sortDown = Icons.SortDown.fixedWidth()
  val sortUp   = Icons.SortUp.fixedWidth()
  val sort     = Icons.Sort.fixedWidth()

  implicit def sortableColElements[D, Plugins](implicit
    ev: Plugins <:< Plugin.SortBy.Tag
  ): SortElements[D, Plugins] =
    new SortElements[D, Plugins] {
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

protected case class SUITableProps[D, Plugins](
  table:        TableTemplate[D, Plugins],
  header:       Boolean | TableHeader,
  headerRow:    TableRow = TableRow(),
  headerCell:   HeaderCell[D, Plugins],
  body:         TableBody,
  row:          RowTemplate[D, Plugins],
  cell:         BodyCell[D, Plugins],
  footer:       Boolean | TableFooter | VdomNode,
  footerRow:    TableRow,
  footerCell:   HeaderCell[D, Plugins]
)(val instance: TableInstance[D, Plugins])

class SUITable[D, Plugins, Layout](
  tableDef:     TableDef[D, Plugins, Layout]
)(implicit
  layout:       LayoutDefaultTag[Layout],
  sortElements: SortElements[D, Plugins]
) {
  val component = ScalaFnComponent[SUITableProps[D, Plugins]] { case props =>
    val tableInstance = props.instance

    val tableRender: TableRender[D, Plugins] = (props.table: Any) match {
      case table: Table =>
        tableInstance => table.copy(as = layout.tag)(tableInstance.getTableProps())
      case other        => other.asInstanceOf[TableRender[D, Plugins]]
    }

    val rowRender: RowRender[D, Plugins] = (props.row: Any) match {
      case row: TableRow => rowData => row.copy(as = layout.tag)(rowData.getRowProps())
      case other         => other.asInstanceOf[RowRender[D, Plugins]]
    }

    val headerTag: Option[TableHeader] = (props.header: Any) match {
      case true  => TableHeader(as = layout.tag).some
      case false => none
      case other => other.asInstanceOf[TableHeader].some // Can't wait for Scala 3's union types
    }

    val headerCellRender: HeaderCellRender[D, Plugins] =
      (props.headerCell: Any) match {
        case headerCell: TableHeaderCell => _ => headerCell.copy(as = layout.tag)
        case fn                          => fn.asInstanceOf[HeaderCellRender[D, Plugins]]
      }

    val bodyCellRender: BodyCellRender[D, Plugins] = (props.cell: Any) match {
      case cell: TableCell => _ => cell.copy(as = layout.tag)
      case fn              => fn.asInstanceOf[BodyCellRender[D, Plugins]]
    }

    val footerCellRender: HeaderCellRender[D, Plugins] = (props.footerCell: Any) match {
      case footerCell: TableHeaderCell => _ => footerCell.copy(as = layout.tag)
      case fn                          => fn.asInstanceOf[HeaderCellRender[D, Plugins]]
    }

    val headerElement: Option[TableHeader] =
      headerTag.map(_(tableInstance.headerGroups.toTagMod { headerRowData =>
        props.headerRow(headerRowData.getHeaderGroupProps())(
          headerRowData.headers.toTagMod((col: tableDef.ColumnType) =>
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
        rowRender(rowData)(
          rowData.cells.toTagMod(cellData =>
            bodyCellRender(cellData)(cellData.getCellProps())(cellData.renderCell)
          )
        )
      }
    )

    def standardFooter(footerTag: TableFooter) =
      footerTag(tableInstance.footerGroups.toTagMod { footerRowData =>
        props.footerRow(footerRowData.getFooterGroupProps())(
          footerRowData.headers.toTagMod((col: tableDef.ColumnType) =>
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

  type Props = SUITableProps[D, Plugins]

  type Component = JsFn.UnmountedWithRoot[Props, Unit, Box[Props]]

  def apply(
    table:      TableTemplate[D, Plugins] = Table(as = layout.tag),
    header:     Boolean | TableHeader = false,
    headerRow:  TableRow = TableRow(as = layout.tag, cellAs = layout.tag),
    headerCell: HeaderCell[D, Plugins] = TableHeaderCell(as = layout.tag),
    body:       TableBody = TableBody(as = layout.tag),
    row:        RowTemplate[D, Plugins] = TableRow(as = layout.tag, cellAs = layout.tag),
    cell:       BodyCell[D, Plugins] = TableCell(as = layout.tag),
    footer:     Boolean | TableFooter | VdomNode = false,
    footerRow:  TableRow = TableRow(as = layout.tag, cellAs = layout.tag),
    footerCell: HeaderCell[D, Plugins] = TableHeaderCell(as = layout.tag)
  ): TableInstance[D, Plugins] => Component = (instance: TableInstance[D, Plugins]) =>
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
}
