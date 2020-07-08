// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import explore.components.ui.GPPStyles
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.collections.menu._

final case class Tile(title: String, movable: Boolean)
    extends ReactPropsWithChildren[Tile](Tile.component)

object Tile {
  type Props = Tile

  implicit val propsReuse = Reusability.derive[Props]

  val component =
    ScalaComponent
      .builder[Props]("Tile")
      .stateless
      .render_PC { (p, c) =>
        <.div(
          Menu(
            clazz = GPPStyles.GPPTileTitle,
            attached = MenuAttached.Top,
            compact = true,
            borderless = true,
            secondary = true,
            tabular = MenuTabular.Right
          )(
            MenuItem(as = "a")(Icons.Bars.when(p.movable), p.title)
          ),
          GPPStyles.GPPTile,
          c
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build

}
