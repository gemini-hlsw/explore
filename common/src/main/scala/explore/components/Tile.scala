// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.syntax.all._
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.components.ui.ExploreStyles._
import explore.implicits._
import explore.model.Constants
import explore.model.enum.TileSizeState
import japgolly.scalajs.react._
import japgolly.scalajs.react.internal.JsUtil
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.reusability._
import org.scalajs.dom.html
import react.common._
import react.common.implicits._
import react.semanticui.collections.menu._
import react.semanticui.elements.button.Button

final case class Tile(
  title:             String,
  back:              Option[Reusable[VdomNode]] = None,
  control:           Option[Reusable[VdomNode]] = None,
  canMinimize:       Boolean = false,
  canMaximize:       Boolean = false,
  state:             TileSizeState = TileSizeState.Normal,
  sizeStateCallback: TileSizeState ~=> Callback = Reusable.always(_ => Callback.empty)
)(val render:        Tile.RenderInTitle ~=> VdomNode)
    extends ReactProps[Tile](Tile.component) {
  def showMaximize: Boolean = canMaximize && state === TileSizeState.Minimized
  def showMinimize: Boolean = canMinimize && state === TileSizeState.Normal
}

object Tile {
  type Props = Tile

  type RenderInTitle = VdomNode ~=> VdomNode

  implicit val propsReuse: Reusability[Tile] = Reusability.derive && Reusability.by(_.render)

  val heightBreakpoints =
    List((200, TileXSH), (700 -> TileSMH), (1024 -> TileMDH))

  val widthBreakpoints  =
    List((300                            -> TileXSW),
         (Constants.TwoPanelCutoff.toInt -> TileSMW),
         (768                            -> TileMDW),
         (1024                           -> TileLGW)
    )

  def renderPortal(mountNode: Option[raw.ReactDOM.Container], info: VdomNode): VdomNode =
    mountNode.map(node => ReactPortal(info, node))

  implicit val rawContainerReuse: Reusability[raw.ReactDOM.Container] = Reusability.always

  class Backend() {

    private val infoRef = Ref.toVdom[html.Element]

    def render(p: Props) = {
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

      <.div(ExploreStyles.Tile)(
        <.div(
          ExploreStyles.TileTitle,
          p.back.map(b => <.div(ExploreStyles.TileButton, b)),
          Menu(
            compact = true,
            borderless = true,
            secondary = true,
            clazz = ExploreStyles.TileTitleMenu
          )(
            MenuItem(as = <.a)(p.title)
          ),
          p.control.map(b => <.div(ExploreStyles.TileControl, b)),
          <.span(ExploreStyles.TileTitleInfo,
                 ^.untypedRef := infoRef,
                 ^.key := s"tile-info-${p.title}"
          ),
          minimizeButton.when(p.showMinimize),
          maximizeButton.when(p.showMaximize)
        ),
        ResponsiveComponent(widthBreakpoints, heightBreakpoints, clazz = ExploreStyles.TileBody)(
          p.render(
            (renderPortal _).reusable(
              JsUtil
                .jsNullToOption(infoRef.raw.current)
                .map(_.asInstanceOf[raw.ReactDOM.Container])
            )
          )
        ).when(p.state =!= TileSizeState.Minimized)
      )
    }
  }

  val component =
    ScalaComponent
      .builder[Props]
      .stateless
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build

}
