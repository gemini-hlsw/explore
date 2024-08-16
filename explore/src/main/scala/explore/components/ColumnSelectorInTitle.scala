// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import crystal.react.View
import cats.syntax.all.*
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import lucuma.react.table.Table
import lucuma.ui.table.ColumnSelector
import lucuma.react.table.ColumnId

case class ColumnSelectorState[A, B](
  table: Option[Table[A, B]] = None
)

case class ColumnSelectorInTitle[A, B](
  columnNames: ColumnId => String,
  state:       View[ColumnSelectorState[A, B]]
) extends ReactFnProps(ColumnSelectorInTitle.component)

object ColumnSelectorInTitle:
  private type Props[A, B] = ColumnSelectorInTitle[A, B]

  private def componentBuilder[A, B] =
    ScalaFnComponent[Props[A, B]]: props =>
      React.Fragment(
        <.span, // Push column selector to right
        <.span(ExploreStyles.TitleSelectColumns)(
          props.state.get.table.map(
            ColumnSelector(_, props.columnNames, ExploreStyles.SelectColumns)
          )
        )
      )

  private val component = componentBuilder[Any, Any]
