// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import explore.Icons
import explore.components.ui.GPPStyles
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.collections.menu._

final case class Tile(title: String, movable: Boolean)
    extends ReactPropsWithChildren[Tile](Tile.component)

object Tile {
  type Props = Tile

  // Explicitly never reuse as we are not considering the content
  implicit val propsReuse: Reusability[Tile] = Reusability.never

  val component =
    ScalaComponent
      .builder[Props]("Tile")
      .stateless
      .render_PC { (p, c) =>
        <.div(
          GPPStyles.GPPTile,
          <.div(
            GPPStyles.GPPTileTitle,
            Menu(
              attached = MenuAttached.Top,
              compact = true,
              borderless = true,
              secondary = true,
              tabular = MenuTabular.Right
            )(
              MenuItem(as = "a")(Icons.Bars.when(p.movable), p.title)
            )
          ),
          <.div(GPPStyles.GPPTileBody, c)
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build

}
