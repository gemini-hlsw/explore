// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.syntax.all._
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.components.ui.ExploreStyles._
import explore.model.enum.TileSizeState
import explore.model.layout
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import org.scalajs.dom
import react.common._
import react.common.implicits._
import react.common.style._
import react.semanticui.collections.menu._
import react.semanticui.elements.button.Button

import scalajs.js
import scalajs.js.|

final case class Tile(
  id:                Tile.TileId,
  title:             String,
  back:              Option[VdomNode] = None,
  control:           Option[VdomNode] = None,
  canMinimize:       Boolean = false,
  canMaximize:       Boolean = false,
  state:             TileSizeState = TileSizeState.Normal,
  sizeStateCallback: TileSizeState => Callback = _ => Callback.empty,
  key:               js.UndefOr[Key] = js.undefined,
  controllerClass:   Option[Css] = None, // applied to wrapping div when in a TileController.
  bodyClass:         Option[Css] = None, // applied to tile body
  tileClass:         Option[Css] = None, // applied to the tile
  tileTitleClass:    Option[Css] = None  // applied to the title
)(val render:        Tile.RenderInTitle => VdomNode)
    extends ReactFnProps[Tile](Tile.component) {
  def showMaximize: Boolean =
    state === TileSizeState.Minimized || (canMaximize && state === TileSizeState.Normal)

  def showMinimize: Boolean =
    state === TileSizeState.Maximized || (canMinimize && state === TileSizeState.Normal)

  def withState(state: TileSizeState, sizeStateCallback: TileSizeState => Callback): Tile =
    copy(state = state, sizeStateCallback = sizeStateCallback)(render)
}

object Tile {
  type Props  = Tile
  type TileId = NonEmptyString

  type RenderInTitle = VdomNode => VdomNode

  val heightBreakpoints =
    List((200, TileXSH), 700 -> TileSMH, 1024 -> TileMDH)

  val widthBreakpoints =
    List(layout.XtraSmallCutoff -> TileXSW,
         layout.SmallCutoff     -> TileSMW,
         layout.MediumCutoff    -> TileMDW,
         layout.LargeCutoff     -> TileLGW
    )

  val component =
    ScalaFnComponent
      .withHooks[Props]
      // infoRef - We use state instead of a regular Ref in order to force a rerender when it's set.
      .useState(none[dom.html.Element])
      .render { (p, infoRef) =>
        val maximizeButton =
          Button(
            as = <.a,
            basic = true,
            compact = true,
            clazz = ExploreStyles.TileStateButton |+| ExploreStyles.BlendedButton,
            onClick = p
              .sizeStateCallback(TileSizeState.Normal)
              .when_(p.state === TileSizeState.Minimized) *> p
              .sizeStateCallback(TileSizeState.Maximized)
              .when_(p.state === TileSizeState.Normal)
          )(Icons.Maximize)

        val minimizeButton =
          Button(
            as = <.a,
            basic = true,
            compact = true,
            clazz = ExploreStyles.TileStateButton |+| ExploreStyles.BlendedButton,
            onClick = p
              .sizeStateCallback(TileSizeState.Normal)
              .when_(p.state === TileSizeState.Maximized) *> p
              .sizeStateCallback(TileSizeState.Minimized)
              .when_(p.state === TileSizeState.Normal)
          )(Icons.Minimize)

        def setInfoRef(node: dom.Node | Null): Unit =
          infoRef
            .modState(_.fold(Option(node.asInstanceOf[dom.html.Element]))(_.some))
            .runNow()

        <.div(
          ExploreStyles.Tile |+| ExploreStyles.FadeIn |+| p.tileClass.orEmpty
        )(
          // Tile title, set classes based on size
          ResponsiveComponent(
            widthBreakpoints,
            heightBreakpoints,
            clazz = ExploreStyles.TileTitle |+| p.tileTitleClass.orEmpty
          )(
            React.Fragment(
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
              <.span(^.key := "tileTitle", ^.untypedRef(setInfoRef).when(infoRef.value.isEmpty))(
                ExploreStyles.TileTitleStrip,
                ExploreStyles.FixedSizeTileTitle.when(!p.canMinimize && !p.canMaximize)
              ),
              <.div(
                minimizeButton.when(p.showMinimize),
                maximizeButton.when(p.showMaximize)
              )
            )
          ),
          // Tile body
          infoRef.value
            .map(node =>
              ResponsiveComponent(
                widthBreakpoints,
                heightBreakpoints,
                clazz = ExploreStyles.TileBody |+| p.bodyClass.orEmpty
              )(
                p.render(info => ReactPortal(info, node))
              ).when(p.state =!= TileSizeState.Minimized)
            )
            .whenDefined
        )
      }
}
