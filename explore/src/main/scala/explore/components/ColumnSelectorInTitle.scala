// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import lucuma.react.table.ColumnId
import lucuma.react.table.ColumnVisibility
import lucuma.react.table.Table
import lucuma.ui.table.ColumnSelector
import crystal.react.View

case class ColumnSelectorInTitle(
  allColumns:       List[(ColumnId, String)],
  columnVisibility: View[ColumnVisibility]
) extends ReactFnProps(ColumnSelectorInTitle.component)

object ColumnSelectorInTitle:
  private type Props = ColumnSelectorInTitle

  private val component =
    ScalaFnComponent[Props]: props =>
      <.span(ExploreStyles.TitleSelectColumns)(
        ColumnSelector(
          props.allColumns,
          props.columnVisibility.get,
          colId => props.columnVisibility.mod(_.toggled(colId)),
          ExploreStyles.SelectColumns
        )
      )
