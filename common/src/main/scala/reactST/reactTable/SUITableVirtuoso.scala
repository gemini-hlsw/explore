// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package reactST.reactTable

import cats.syntax.all._
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.common.implicits._
import react.common.style.Css
import react.semanticui.collections.table._
import react.virtuoso._
import react.virtuoso.raw.ListRange
import reactST.reactTable.facade.tableInstance.TableInstance

import scalajs.js
import scalajs.js.|
import scalajs.js.JSConverters._
import definitions._

class SUITableVirtuoso[D, Plugins](
  tableDef:     TableDef[D, Plugins, Layout.NonTable] // tableDef is just used to infer types.
)(implicit
  sortElements: SortElements[D, Plugins]
) {
  // I expect this to be inferred in Scala 3
  type RefType =
    facade.React.Component[GroupedVirtuoso.GroupedVirtuosoProps, Null] with VirtuosoComponent

  case class Component(
    table:              TableTemplate[D, Plugins] = Table(): TableTemplate[D, Plugins],
    header:             Boolean | TableHeader = false,
    headerRow:          TableRow = TableRow(),
    headerCell:         HeaderCell[D, Plugins] = TableHeaderCell(): HeaderCell[D, Plugins],
    body:               TableBody = TableBody()(^.height := "100%"),
    row:                RowTemplate[D, Plugins] = TableRow(): RowTemplate[D, Plugins],
    cell:               BodyCell[D, Plugins] = TableCell(): BodyCell[D, Plugins],
    footer:             Boolean | TableFooter | VdomNode = false,
    footerRow:          TableRow = TableRow(),
    footerCell:         HeaderCell[D, Plugins] = TableHeaderCell(): HeaderCell[D, Plugins],
    emptyMessage:       String = ""
  )(
    val instance:       TableInstance[D, Plugins],
    val initialIndex:   Option[Int] = None,
    val rangeChanged:   Option[ListRange => Callback] = None,
    val atTopChange:    Option[Boolean => Callback] = None,
    val atBottomChange: Option[Boolean => Callback] = None
  ) extends ReactPropsForwardRef[Component, RefType](component)

  def createRef = Ref.toJsComponent(GroupedVirtuoso.component)

  val component =
    React.forwardRef.toJsComponent(GroupedVirtuoso.component)[Component] { (props, ref) =>
      val tableInstance = props.instance

      def addClass(className: js.UndefOr[String], clazz: js.UndefOr[Css], newClass: Css): Css =
        className.map(Css.apply).orElse(clazz).fold(newClass)(_ |+| newClass)

      val tableRender: TableRender[D, Plugins] = (props.table: Any) match {
        case table: Table =>
          tableInstance =>
            table.copy(as = <.div,
                       clazz = addClass(table.className, table.clazz, ExploreStyles.Table)
            )(tableInstance.getTableProps())
        case fn           =>
          tableInstance =>
            val table = fn.asInstanceOf[TableRender[D, Plugins]](tableInstance)
            table
              .copy(as = <.div, clazz = addClass(table.className, table.clazz, ExploreStyles.Table))
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
        case _                   => none
      }

      val headerRowTag: TableRow = props.headerRow.copy(
        as = <.div,
        cellAs = <.div,
        clazz = addClass(props.headerRow.className, props.headerRow.clazz, ExploreStyles.TR)
      )

      val headerCellRender: HeaderCellRender[D, Plugins] =
        (props.headerCell: Any) match {
          case headerCell: TableHeaderCell =>
            _ =>
              headerCell.copy(
                as = <.div,
                clazz = addClass(headerCell.className, headerCell.clazz, ExploreStyles.TH)
              )
          case fn                          =>
            colInstance =>
              val headerCell =
                fn.asInstanceOf[HeaderCellRender[D, Plugins]](colInstance)
              headerCell.copy(
                as = <.div,
                clazz = addClass(headerCell.className, headerCell.clazz, ExploreStyles.TH)
              )
        }

      def rowIndexCss(index: Int): Css =
        ExploreStyles.TR |+| (index % 2 match {
          case 0 => ExploreStyles.EvenRow
          case 1 => ExploreStyles.OddRow
        })

      val rowRender: RowRender[D, Plugins] = (props.row: Any) match {
        case row: TableRow =>
          rowData =>
            row.copy(as = <.div,
                     cellAs = <.div,
                     clazz = addClass(row.className, row.clazz, rowIndexCss(rowData.index.toInt))
            )(rowData.getRowProps())
        case fn            =>
          rowData =>
            val row = fn.asInstanceOf[RowRender[D, Plugins]](rowData)
            row.copy(as = <.div,
                     cellAs = <.div,
                     clazz = addClass(row.className, row.clazz, rowIndexCss(rowData.index.toInt))
            )
      }

      val bodyCellRender: BodyCellRender[D, Plugins] = (props.cell: Any) match {
        case bodyCell: TableCell =>
          _ =>
            bodyCell.copy(as = <.div,
                          clazz = addClass(bodyCell.className, bodyCell.clazz, ExploreStyles.TD)
            )
        case fn                  =>
          cell =>
            val bodyCell = fn.asInstanceOf[BodyCellRender[D, Plugins]](cell)
            bodyCell.copy(as = <.div,
                          clazz = addClass(bodyCell.className, bodyCell.clazz, ExploreStyles.TD)
            )
      }

      val footerCellRender: HeaderCellRender[D, Plugins] =
        (props.footerCell: Any) match {
          case footerCell: TableHeaderCell =>
            _ =>
              footerCell.copy(
                as = <.div,
                clazz = addClass(footerCell.className, footerCell.clazz, ExploreStyles.TH)
              )
          case fn                          =>
            colInstance =>
              val footerCell =
                fn.asInstanceOf[HeaderCellRender[D, Plugins]](colInstance)
              footerCell.copy(
                as = <.div,
                clazz = addClass(footerCell.className, footerCell.clazz, ExploreStyles.TH)
              )
        }

      val headerElement: Option[TableHeader] =
        headerTag.map(_(tableInstance.headerGroups.toTagMod { headerRowData =>
          headerRowTag(headerRowData.getHeaderGroupProps())(
            headerRowData.headers.toTagMod((col: tableDef.ColumnType) =>
              headerCellRender(col)(col.getHeaderProps(), sortElements.props(col))(
                col.renderHeader,
                sortElements.indicator(col)
              )
            )
          )
        }))

      val renderRow = (index: Int, _: Int) => {
        val rowData = tableInstance.rows(index)
        tableInstance.prepareRow(rowData)
        rowRender(rowData)(
          rowData.cells.toTagMod(cellData =>
            bodyCellRender(cellData)(cellData.getCellProps())(cellData.renderCell)
          )
        ).render.vdomElement
      }

      val bodyElement: TableBody =
        props.body.copy(as = <.div)(tableInstance.getTableBodyProps())(
          TagMod.when(tableInstance.rows.nonEmpty)(
            GroupedVirtuoso(
              itemContent = renderRow,
              groupCounts = List(tableInstance.rows.length),
              groupContent =
                headerElement.map(header => (_: Int) => header.vdomElement).orUndefined,
              initialTopMostItemIndex = props.initialIndex.orUndefined,
              rangeChanged = props.rangeChanged.orUndefined,
              atTopStateChange = props.atTopChange.orUndefined,
              atBottomStateChange = props.atBottomChange.orUndefined
            )(ExploreStyles.TBody).withOptionalRef(ref)
          ),
          TagMod.when(tableInstance.rows.isEmpty)(props.emptyMessage)
        )

      def standardFooter(footerTag: TableFooter) =
        footerTag.copy(as = <.div,
                       clazz = addClass(footerTag.className, footerTag.clazz, ExploreStyles.TR)
        )
      tableInstance.footerGroups.toTagMod { footerRowData =>
        props.footerRow.copy(as = <.div, cellAs = <.div)(footerRowData.getFooterGroupProps())(
          footerRowData.headers.toTagMod((col: tableDef.ColumnType) =>
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
        bodyElement,
        footerElement
      ).vdomElement
    }
}
