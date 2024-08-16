// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.components.ui.ExploreStyles.*
import explore.model.enums.TileSizeState
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import lucuma.react.common.style.*
import lucuma.react.primereact.Button
import lucuma.ui.syntax.all.given
import org.scalajs.dom

case class Tile[A](
  id:                Tile.TileId,
  initialState:      A,
  title:             String,
  back:              Option[VdomNode] = None,
  canMinimize:       Boolean = true,
  canMaximize:       Boolean = true,
  hidden:            Boolean = false,
  sizeState:         TileSizeState = TileSizeState.Maximized,
  sizeStateCallback: TileSizeState => Callback = _ => Callback.empty,
  controllerClass:   Css = Css.Empty, // applied to wrapping div when in a TileController.
  bodyClass:         Css = Css.Empty, // applied to tile body
  tileClass:         Css = Css.Empty, // applied to the tile
  tileTitleClass:    Css = Css.Empty  // applied to the title
)(
  val tileBody:      View[A] => VdomNode,
  val tileTitle:     (View[A], TileSizeState) => VdomNode = (_: View[A], _: TileSizeState) => EmptyVdom
) extends ReactFnProps(Tile.component) {
  val fullSize: Boolean = !canMinimize && !canMaximize

  def showMaximize: Boolean =
    sizeState === TileSizeState.Minimized || (canMaximize && sizeState === TileSizeState.Minimized)

  def showMinimize: Boolean =
    sizeState === TileSizeState.Maximized || (canMinimize && sizeState === TileSizeState.Maximized)

  def withState(
    state:             TileSizeState,
    sizeStateCallback: TileSizeState => Callback
  ): Tile[A] =
    copy(sizeState = state, sizeStateCallback = sizeStateCallback)(tileBody, tileTitle)

  def withBackButton(
    back: Option[VdomNode]
  ): Tile[A] =
    copy(back = back)(tileBody, tileTitle)
}

object Tile:
  type TileId        = NonEmptyString
  type RenderInTitle = VdomNode => VdomNode

  private type Props[A] = Tile[A]

  private def componentBuilder[A] =
    ScalaFnComponent
      .withHooks[Props[A]]
      .useStateViewBy(_.initialState)
      // infoRef - We use state instead of a regular Ref in order to force a rerender when it's set.
      .useState(none[dom.html.Element])
      .render: (p, sharedState, infoRef) =>
        val maximizeButton =
          Button(
            text = true,
            clazz = ExploreStyles.TileStateButton,
            icon = Icons.Maximize,
            onClick = p
              .sizeStateCallback(TileSizeState.Maximized)
              .when_(p.sizeState === TileSizeState.Minimized)
          )

        val minimizeButton =
          Button(
            text = true,
            clazz = ExploreStyles.TileStateButton,
            icon = Icons.Minimize,
            onClick = p
              .sizeStateCallback(TileSizeState.Minimized)
              .when_(p.sizeState === TileSizeState.Maximized)
          )

        def setInfoRef(node: dom.Node | Null): Unit =
          infoRef
            .modState(_.fold(Option(node.asInstanceOf[dom.html.Element]))(_.some))
            .runNow()

        if (!p.hidden) {
          <.div(
            ExploreStyles.Tile |+| ExploreStyles.FadeIn |+| p.tileClass,
            ^.key := p.id.value
          )(
            // Tile title
            <.div(ExploreStyles.TileTitle)(
              <.div(
                ExploreStyles.TileTitleMenu |+| p.tileTitleClass,
                p.back.map(b => <.div(ExploreStyles.TileButton, b)),
                <.div(ExploreStyles.TileTitleText |+| ExploreStyles.TileDraggable, p.title)
              ),
              <.div(
                ExploreStyles.TileTitleControlArea,
                <.div(ExploreStyles.TileTitleStrip |+| ExploreStyles.TileControl,
                      p.tileTitle(sharedState, p.sizeState)
                ),
                <.div(^.key := s"tileTitle-${p.id.value}",
                      ^.untypedRef(setInfoRef).when(infoRef.value.isEmpty)
                )(
                  ExploreStyles.TileTitleStrip,
                  ExploreStyles.FixedSizeTileTitle.when(!p.canMinimize && !p.canMaximize)
                )
              ),
              <.div(ExploreStyles.TileControlButtons,
                    minimizeButton.when(p.showMinimize && !p.fullSize),
                    maximizeButton.when(p.showMaximize && !p.fullSize)
              )
            ),
            // Tile body
            infoRef.value
              .map(node =>
                <.div(ExploreStyles.TileBody |+| p.bodyClass, p.tileBody(sharedState))
                  .when(p.sizeState =!= TileSizeState.Minimized)
              )
              .whenDefined
          )
        } else EmptyVdom

  private val component = componentBuilder[Any]
