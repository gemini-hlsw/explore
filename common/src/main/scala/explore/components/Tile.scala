// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import explore.Icons
import explore.components.ui.ExploreStyles
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.semanticui.collections.menu._

final case class TileButton(body: VdomNode)

final case class Tile(title: String, movable: Boolean, back: Option[TileButton])
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
          ExploreStyles.Tile,
          <.div(
            ExploreStyles.TileTitle,
            Menu(
              attached = MenuAttached.Top,
              compact = true,
              borderless = true,
              secondary = true,
              clazz = ExploreStyles.TileTitleMenu,
              tabular = MenuTabular.Right
            )(
              p.back.map(b => <.div(ExploreStyles.TileButton, b.body)),
              MenuItem(as = "a")(Icons.Bars.when(p.movable), p.title)
            )
          ),
          <.div(ExploreStyles.TileBody, c)
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build

}
