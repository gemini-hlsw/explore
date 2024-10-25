// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import lucuma.react.table.ColumnId
import lucuma.react.table.Table
import lucuma.ui.table.ColumnSelector

case class ColumnSelectorInTitle[T, TM](
  columnNames: ColumnId => Option[String],
  table:       Option[Table[T, TM]]
) extends ReactFnProps(ColumnSelectorInTitle.component)

object ColumnSelectorInTitle:
  private type Props[A, B] = ColumnSelectorInTitle[A, B]

  private def componentBuilder[A, B] =
    ScalaFnComponent[Props[A, B]]: props =>
      React.Fragment(
        <.span, // Push column selector to right
        <.span(ExploreStyles.TitleSelectColumns)(
          props.table.map:
            ColumnSelector(_, props.columnNames, ExploreStyles.SelectColumns)
        )
      )

  private val component = componentBuilder[Any, Any]
