// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package reactST.reactTable

import cats.syntax.all._
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.JsFn
import japgolly.scalajs.react.internal.Box
import japgolly.scalajs.react.vdom.html_<^._
import react.common.implicits._
import react.common.style.Css
import react.semanticui.collections.table._
import react.virtuoso.GroupedVirtuoso
import reactST.reactTable._
import reactST.reactTable.mod.{ ^ => _, _ }
import reactST.reactTable.syntax._

import scala.annotation.unused

import scalajs.js
import scalajs.js.|
import scalajs.js.JSConverters._
import definitions._

protected case class SUITableVirtuosoProps[D, TableInstanceD <: TableInstance[
  D
], ColumnInstanceD <: ColumnObject[D]](
  table:        TableTemplate[D, TableInstanceD],
  header:       Boolean | TableHeader,
  headerRow:    TableRow = TableRow(),
  headerCell:   HeaderCell[D, ColumnInstanceD],
  body:         TableBody,
  row:          RowTemplate[D],
  cell:         BodyCell[D],
  footer:       Boolean | TableFooter | VdomNode,
  footerRow:    TableRow,
  footerCell:   HeaderCell[D, ColumnInstanceD]
)(val instance: TableInstanceD)

class SUITableVirtuoso[
  D,
  TableOptsD <: UseTableOptions[D],
  TableInstanceD <: TableInstance[D],
  ColumnOptsD <: ColumnOptions[D],
  ColumnInstanceD <: ColumnObject[D],
  State <: TableState[D] // format: on
]( // tableDef is used to infer types.
  @unused tableDef: TableDef[
    D,
    TableOptsD,
    TableInstanceD,
    ColumnOptsD,
    ColumnInstanceD,
    State,
    Layout.NonTable
  ]
)(implicit
  sortElements:     SortElements[ColumnInstanceD]
) {
  val component = ScalaFnComponent[SUITableVirtuosoProps[D, TableInstanceD, ColumnInstanceD]] {
    case props =>
      val tableInstance = props.instance

      def addClass(className: js.UndefOr[String], clazz: js.UndefOr[Css], newClass: Css): Css =
        className.map(Css.apply).orElse(clazz).fold(newClass)(_ |+| newClass)

      val tableRender: TableRender[D, TableInstanceD] = (props.table: Any) match {
        case table: Table =>
          tableInstance =>
            table.copy(as = <.div,
                       clazz = addClass(table.className, table.clazz, ExploreStyles.Table)
            )(tableInstance.getTableProps())
        case fn           =>
          tableInstance =>
            val table = fn.asInstanceOf[TableRender[D, TableInstanceD]](tableInstance)
            table.copy(as = <.div,
                       clazz = addClass(table.className, table.clazz, ExploreStyles.Table)
            )
      }

      val headerTag: Option[TableHeader] = (props.header: Any) match {
        case true                => TableHeader(as = <.div, clazz = ExploreStyles.THead).some
        case header: TableHeader =>
          (header
            .asInstanceOf[TableHeader]
            .copy(as = <.div,
                  clazz = addClass(header.className, header.clazz, ExploreStyles.THead)
            ))
            .some // Can't wait for Scala 3's union types
        case _ => none
      }

      val headerRowTag: TableRow = props.headerRow.copy(
        as = <.div,
        cellAs = <.div,
        clazz = addClass(props.headerRow.className, props.headerRow.clazz, ExploreStyles.TR)
      )

      val headerCellRender: HeaderCellRender[D, ColumnInstanceD] =
        (props.headerCell: Any) match {
          case headerCell: TableHeaderCell =>
            _ =>
              headerCell.copy(as = <.div,
                              clazz =
                                addClass(headerCell.className, headerCell.clazz, ExploreStyles.TH)
              )
          case fn                          =>
            colInstance =>
              val headerCell = fn.asInstanceOf[HeaderCellRender[D, ColumnInstanceD]](colInstance)
              headerCell.copy(as = <.div,
                              clazz =
                                addClass(headerCell.className, headerCell.clazz, ExploreStyles.TH)
              )
        }

      val rowRender: RowRender[D] = (props.row: Any) match {
        case row: TableRow =>
          rowData =>
            row.copy(as = <.div,
                     cellAs = <.div,
                     clazz = addClass(row.className, row.clazz, ExploreStyles.TR)
            )(rowData.getRowProps())
        case fn            =>
          rowData =>
            val row = fn.asInstanceOf[RowRender[D]](rowData)
            row.copy(as = <.div,
                     cellAs = <.div,
                     clazz = addClass(row.className, row.clazz, ExploreStyles.TR)
            )
      }

      val bodyCellRender: BodyCellRender[D] = (props.cell: Any) match {
        case bodyCell: TableCell =>
          _ =>
            bodyCell.copy(as = <.div,
                          clazz = addClass(bodyCell.className, bodyCell.clazz, ExploreStyles.TD)
            )
        case fn                  =>
          cell =>
            val bodyCell = fn.asInstanceOf[BodyCellRender[D]](cell)
            bodyCell.copy(as = <.div,
                          clazz = addClass(bodyCell.className, bodyCell.clazz, ExploreStyles.TD)
            )
      }

      val footerCellRender: HeaderCellRender[D, ColumnInstanceD] = (props.footerCell: Any) match {
        case footerCell: TableHeaderCell =>
          _ =>
            footerCell.copy(as = <.div,
                            clazz =
                              addClass(footerCell.className, footerCell.clazz, ExploreStyles.TH)
            )
        case fn                          =>
          colInstance =>
            val footerCell = fn.asInstanceOf[HeaderCellRender[D, ColumnInstanceD]](colInstance)
            footerCell.copy(as = <.div,
                            clazz =
                              addClass(footerCell.className, footerCell.clazz, ExploreStyles.TH)
            )
      }

      val headerElement: Option[TableHeader] =
        headerTag.map(_(tableInstance.headerGroups.toTagMod { headerRowData =>
          headerRowTag(headerRowData.getHeaderGroupProps())(
            TableDef
              .headersFromGroup(headerRowData)
              .toTagMod((col: ColumnInstanceD) =>
                headerCellRender(col)(col.getHeaderProps(), sortElements.props(col))(
                  col.renderHeader,
                  sortElements.indicator(col)
                )
              )
          )
        }))

      val renderRow = (_: Int, _: Int, rowData: Row[D]) => {
        tableInstance.prepareRow(rowData)
        rowRender(rowData)(
          rowData.cells.toTagMod(cellData =>
            bodyCellRender(cellData)(cellData.getCellProps())(cellData.renderCell)
          )
        ).render.vdomElement
      }

      val bodyElement: TableBody = props.body.copy(as = <.div)(tableInstance.getTableBodyProps())(
        TagMod.when(tableInstance.rows.nonEmpty)(
          GroupedVirtuoso[Row[D]](
            data = tableInstance.rows,
            itemContent = renderRow,
            groupContent = headerElement.map(header => (_: Int) => header.vdomElement).orUndefined
          )(ExploreStyles.TBody)
        ),
        TagMod.when(tableInstance.rows.isEmpty)("No matching modes")
      )

      def standardFooter(footerTag: TableFooter) =
        footerTag.copy(as = <.div,
                       clazz = addClass(footerTag.className, footerTag.clazz, ExploreStyles.TR)
        )
      tableInstance.footerGroups.toTagMod { footerRowData =>
        props.footerRow.copy(as = <.div, cellAs = <.div)(footerRowData.getFooterGroupProps())(
          TableDef
            .headersFromGroup(footerRowData)
            .toTagMod((col: ColumnInstanceD) =>
              footerCellRender(col)(col.getFooterProps())(col.renderFooter)
            )
        )
      }

      val footerElement: Option[VdomNode] = (props.footer: Any) match {
        case true                   => standardFooter(TableFooter()).vdomElement.some
        case false                  => none
        case footer: TableFooter    => standardFooter(footer).vdomElement.some
        case otherElement: VdomNode => otherElement.some
        case _                      => ??? // Can't wait for Scala 3's union types
      }

      tableRender(tableInstance)(
        // headerElement,
        bodyElement,
        footerElement
      ).vdomElement
  }

  type Props = SUITableVirtuosoProps[D, TableInstanceD, ColumnInstanceD]

  type Component = JsFn.UnmountedWithRoot[Props, Unit, Box[Props]]

  def apply(
    table:      TableTemplate[D, TableInstanceD] = Table(),
    header:     Boolean | TableHeader = false,
    headerRow:  TableRow = TableRow(),
    headerCell: HeaderCell[D, ColumnInstanceD] = TableHeaderCell(),
    body:       TableBody = TableBody()(^.height := "100%"),
    row:        RowTemplate[D] = TableRow(),
    cell:       BodyCell[D] = TableCell(),
    footer:     Boolean | TableFooter | VdomNode = false,
    footerRow:  TableRow = TableRow(),
    footerCell: HeaderCell[D, ColumnInstanceD] = TableHeaderCell()
  ): TableInstanceD => Component = (instance: TableInstanceD) =>
    component(
      SUITableVirtuosoProps(table,
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
