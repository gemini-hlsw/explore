// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.syntax.all._
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.components.ui.ExploreStyles._
import explore.model.enum.TileSizeState
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.common.implicits._
import react.resizeDetector.ResizeDetector
import react.semanticui.collections.menu._
import react.semanticui.elements.button.Button
import explore.model.Constants

final case class TileButton(body: VdomNode)

final case class Tile(
  title:             String,
  back:              Option[TileButton] = None,
  canMinimize:       Boolean = false,
  canMaximize:       Boolean = false,
  state:             TileSizeState = TileSizeState.Normal,
  sizeStateCallback: TileSizeState => Callback = _ => Callback.empty
) extends ReactPropsWithChildren[Tile](Tile.component) {
  def showMaximize: Boolean = canMaximize && state === TileSizeState.Minimized
  def showMinimize: Boolean = canMinimize && state === TileSizeState.Normal
}

object Tile {
  type Props = Tile

  // Explicitly never reuse as we are not considering the content
  implicit val propsReuse: Reusability[Tile] = Reusability.never
  val heightBreakpoints                      =
    List((200, TileXSH), (700 -> TileSMH), (1024 -> TileMDH))

  val widthBreakpoints                       =
    List((300                            -> TileXSW),
         (Constants.TwoPanelCutoff.toInt -> TileSMW),
         (768                            -> TileMDW),
         (1024                           -> TileLGW)
    )

  val component =
    ScalaComponent
      .builder[Props]
      .stateless
      .render_PC { (p, c) =>
        ResizeDetector() { s =>
          val maximizeButton =
            Button(
              as = <.a,
              basic = true,
              compact = true,
              clazz = ExploreStyles.TileStateButton |+| ExploreStyles.BlendedButton,
              onClick = p.sizeStateCallback(TileSizeState.Maximized)
            )(Icons.Maximize.fitted(true))

          val minimizeButton =
            Button(
              as = <.a,
              basic = true,
              compact = true,
              clazz = ExploreStyles.TileStateButton |+| ExploreStyles.BlendedButton,
              onClick = p.sizeStateCallback(TileSizeState.Minimized)
            )(Icons.Minimize.fitted(true))

          <.div(ExploreStyles.Tile, s.targetRef)(
            <.div(
              ExploreStyles.TileTitle,
              p.back.map(b => <.div(ExploreStyles.TileButton, b.body)),
              Menu(
                attached = MenuAttached.Top,
                compact = true,
                borderless = true,
                secondary = true,
                clazz = ExploreStyles.TileTitleMenu,
                tabular = MenuTabular.Right
              )(
                MenuItem(as = <.a)(p.title)
              ),
              minimizeButton.when(p.showMinimize),
              maximizeButton.when(p.showMaximize)
            ),
            ResponsiveComponent(widthBreakpoints,
                                heightBreakpoints,
                                clazz = ExploreStyles.TileBody
            )(c).when(p.state =!= TileSizeState.Minimized)
          )
        }
      }
      .configure(Reusability.shouldComponentUpdate)
      .build

}
