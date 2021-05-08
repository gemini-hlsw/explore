// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package reactST.reactTable

import japgolly.scalajs.react.internal.JsUtil
import japgolly.scalajs.react.raw
import japgolly.scalajs.react.raw.React.ComponentClassP
import japgolly.scalajs.react.vdom._
import reactST.reactTable.anon.Data
import reactST.reactTable.mod.ColumnInterfaceBasedOnValue._
import reactST.reactTable.mod.{ ^ => _, _ }
import reactST.std.Partial

import scalajs.js
import scalajs.js.|
import scalajs.js.JSConverters._

// format: off
trait TableMaker2[
  D,
  TableOptsD <: UseTableOptions[D], 
  TableInstanceD <: TableInstance[D], 
  ColumnOptsD <: ColumnWithLooseAccessor[D], 
  ColumnInstanceD <: ColumnInstance[D], 
  State <: TableState[D]] { self =>
  // format: on

  import syntax._

  val plugins: List[PluginHook[D]]

  /**
   * Create an empty instance of TableOptsD.
   */
  def emptyOptions: TableOptsD = js.Dynamic.literal().asInstanceOf[TableOptsD]

  /**
   * Create a TableOptsD with a row id function and columns set.
   */
  def options(rowIdFn: D => String, columns: js.Array[Column[D]]): TableOptsD =
    emptyOptions.setRowIdFn(rowIdFn).setColumns(columns)

  /**
   * Create an empty instance of ColumnOptsD.
   * The other column creating methods are usually a better starting place.
   */
  def emptyColumn: ColumnOptsD = js.Dynamic.literal().asInstanceOf[ColumnOptsD]

  /**
   * Create a ColumnOptsD setup up for a simple column with an accessor function.
   *
   * @param id The unique id for the column.
   * @param accessor Accessor function to get the column value from the row value.
   */
  def accessorColumn[V](id: String, accessor: D => V): ColumnOptsD =
    emptyColumn.setId(id).setAccessorFn(accessor)

  // return type of a scalajs-react component function.
  type ComponentFnResult = raw.ChildrenArray[raw.Empty | String | raw.JsNumber | raw.React.Element]

  /**
   * Create a column with a scalajs-react component in it.
   *
   * @param id The unique id for the column.
   * @param component The scalajs-component (see notes below)
   *
   * Notes:
   * The component is a ScalaJsComponent on which
   *   ".cmapCtorProps[(CellProps[RowData, A]) with js.Object].jsComponent.raw"
   *   has been called.
   * Strangely enough, for a sortable column you still need to provide an accessor,
   *   even if you provide a sorting function or the column will not be sortable. Seems
   *   like a possible bug in react-table.
   */
  def componentColumn[V](
    id:        String,
    component: js.Function1[CellProps[D, V], ComponentFnResult]
  ): ColumnOptsD = {
    type ColBasedOnValue = ColumnOptsD with ColumnInterfaceBasedOnValue[D, V]
    val col = emptyColumn.setId(id).asInstanceOf[ColBasedOnValue]

    // This should be an implicit call, but I can't get the types to line up
    val c = new ColumnInterfaceBasedOnValueMutableBuilder[ColBasedOnValue, D, V](col)
    c.setCellComponentClass(
      component.asInstanceOf[ComponentClassP[(CellProps[D, V]) with js.Object]]
    )
  }

  /**
   * Create a column group with the specified columns in it.
   *
   * @param header The header for the column group. Seems to be required.
   * @param cols The columns to include in the group.
   */
  def columnGroup(header: Renderer[HeaderProps[D]], cols: ColumnOptsD*): ColumnGroup[D] =
    js.Dynamic
      .literal()
      .asInstanceOf[ColumnGroup[D]]
      .setHeader(header)
      .setColumns(cols.toJSArray.asInstanceOf[js.Array[Column[D]]])

  /**
   * Create an array of columns and column groups from the varargs parameters.
   *
   * @param cols A varargs parameter of columns or column groups.
   */
  def columnArray(cols: (ColumnGroup[D] | ColumnOptsD)*): js.Array[Column[D]] =
    cols.toJSArray.asInstanceOf[js.Array[Column[D]]]

  /**
   * Create an empty instance of type State
   */
  def emptyState: State = js.Dynamic.literal().asInstanceOf[State]

  /**
   * Create a TableInstanceD instancy by calling useTable with the
   * provided options and the plugins that have been configured by
   * the with* methods.
   *
   * This is used internally by makeTable, but may be useful for
   * creating custom tables.
   *
   * @param options The table options.
   */
  def use(options: TableOptsD): TableInstanceD =
    Hooks.useTable(options, plugins: _*).asInstanceOf[TableInstanceD]

  /*
   * When adding new plugins, see https://github.com/DefinitelyTyped/DefinitelyTyped/tree/master/types/react-table
   * for help determining what the necessary type changes are.
   *
   * Hooks need to have facades created in Hooks.scala, as the facades created by
   * ScalablyTyped won't work as hooks.
   *
   * Heads Up! It doesn't seem to be documented, but the order of plugins seems
   * to matter. If you put useSortBy before useFilters you will get a runtime error.
   * We may want to add some means of ordering the plugs when we submit them to
   * "useTable" if we can figure out what the required order is.
   */

  /**
   * Add sort capabilities to the table via the useSortBy plugin hook.
   */
  // format: off
  // def withSort = new TableMaker[
  //   D, 
  //   TableOptsD with UseSortByOptions[D], 
  //   TableInstanceD with UseSortByInstanceProps[D], 
  //   ColumnOptsD with UseSortByColumnOptions[D], 
  //   ColumnInstanceD with UseSortByColumnProps[D], 
  //   State with UseSortByState[D]] {
  //   val plugins = self.plugins :+ Hooks.useSortBy.asInstanceOf[PluginHook[D]]
  // }
  // format: on

  /**
   * Adds support for headers and cells to be rendered as inline-block divs
   * (or other non-table elements) with explicit width. This becomes useful if and when you need
   * to virtualize rows and cells for performance.
   *
   * NOTE: Although no additional options are needed for this plugin to work, the core column
   * options width, minWidth and maxWidth are used to calculate column and cell widths and must
   * be set.
   */
  // format: off
  // def withBlockLayout = new TableMaker[
  //   D, 
  //   TableOptsD with LayoutMarker, // A marker trait to limit calls to makeVirtualizedTable.
  //   TableInstanceD, 
  //   ColumnOptsD, 
  //   ColumnInstanceD, 
  //   State] {
  //   val plugins = self.plugins :+ Hooks.useBlockLayout.asInstanceOf[PluginHook[D]]
  // }
  // format: on

  protected def headersFromGroup(headerGroup: HeaderGroup[D]) =
    headerGroup.headers.asInstanceOf[js.Array[ColumnInstanceD]]

  // When trying to use a more traditional "syntax" package and implicit
  // classes, the compiler seems to somehow loose type information along
  // the way. Putting the syntax here allows resolution to work correctly.
  object syntax {
    implicit class TableOptionOps(val table: TableOptsD) {

      /**
       * Sets the initial state of the table.
       *
       * The provided setInitialState method takes a Partial[TableState[D]]
       */
      def setInitialStateFull(s: TableState[D]): TableOptsD =
        table.setInitialState(s.asInstanceOf[Partial[TableState[D]]])

      /**
       * Sets the row id for the rows of the table based on a function.
       *
       * @param f A function from the row type to the row id.
       */
      def setRowIdFn(f: D => String): TableOptsD =
        table.setGetRowId((data, _, _) => f(data))
    }

    implicit class ColumnOptionOps(val col: ColumnOptsD) {

      /**
       * Sets the accessorFunction for the column.
       *
       * @param f A function from the row type to the column type.
       */
      def setAccessorFn[V](f: D => V): ColumnOptsD =
        col.setAccessorFunction3(TableMaker.accessorFn(f))

      /**
       * Sets the sorting for the column based on a function.
       *
       * @param f A function from the row type to the column type.
       * @param ordering An implicit ordering for the column type.
       * @param evidence Evidence that this column is sortable. (See note)
       *
       * Note:
       * This method is only valid for columns that are sortable via the
       *   useSortBy plugin. The compiler was unable to resolve the types if
       *   an implicit class requiring UseSortByColumnOptions[D] was used, so
       *   I switched to requiring evidence that ColumnOptsD is a subtype
       *   of UseSortByColumnOptions[D] and that worked. Unfortunately, requires
       *   asInstanceOfs.
       */
      def setSortByFn[V](
        f:                 D => V
      )(implicit ordering: Ordering[V], evidence: ColumnOptsD <:< UseSortByColumnOptions[D]) = {
        val sbfn: SortByFn[D] = (d1, d2, _, _) =>
          ordering.compare(f(d1.original), f(d2.original)).toDouble
        // these asInstanceOfs should be safe, but is there a way around them?
        col.asInstanceOf[UseSortByColumnOptions[D]].setSortType(sbfn).asInstanceOf[ColumnOptsD]
      }
    }
  }
}

object TableMaker2 {
  // format: off
  // def apply[D] = new TableMaker[
  //   D, 
  //   UseTableOptions[D], 
  //   TableInstance[D], 
  //   ColumnWithLooseAccessor[D], 
  //   ColumnInstance[D], 
  //   TableState[D]] {
  //     val plugins = List.empty
  //   }
    // format: on

  /**
   * Helper method for taking an object return by a react-table
   * and turning it into a TagMod of attributes.
   */
  def props2Attrs(obj: js.Object): TagMod = TagMod.fn { b =>
    for ((k, v) <- JsUtil.objectIterator(obj)) b.addAttr(k, v)
  }

  def accessorFn[D, V](f: D => V): js.Function3[D, Double, Data[D], js.Any] =
    (data, _, _) => f(data).asInstanceOf[js.Any]

  // trait LayoutMarker
}
