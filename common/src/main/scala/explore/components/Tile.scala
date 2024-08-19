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

/**
 * In explore we have a concept of a Tile which is a draggable and resizable window that can contain
 * any content. Tiles can be minimized, maximized and resized and dragged from the title Often we
 * want to render content in the body and on the title of the tile, thus you can pass a tileBody and
 * tileTitle function to the Tile constructor returning a VdomNode. The body and title may need to
 * share some state of type A, you can pass an initial value and the body and title will get a
 * View[A] to access the state and modify it.
 *
 * @param id
 *   a unique identifier for the tile
 * @param initialState
 *   the initial state of the tile
 * @param title
 *   the title of the tile to be rendered in the title bar
 * @param back
 *   an optional back button to be rendered in the title bar
 * @param canMinimize
 *   whether the tile can be minimized
 * @param canMaximize
 *   whether the tile can be maximized
 * @param hidden
 *   whether the tile is hidden
 * @param sizeState
 *   the initial size state of the tile
 * @param sizeStateCallback
 *   a callback to be called when the size state changes
 * @param controllerClass
 *   a css class to be applied to the wrapping div when in a TileController
 * @param bodyClass
 *   a css class to be applied to the tile body
 * @param tileClass
 *   a css class to be applied to the tile
 * @param tileTitleClass
 *   a css class to be applied to the title
 *
 * @param tileBody
 *   a function to render the body of the tile, it receives a View[A] to access the state
 * @param tileTitle
 *   a function to render the title of the tile, it receives a View[A] and the current size state
 */
case class Tile[A](
  id:                Tile.TileId,
  title:             String,
  initialState:      A = (),
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
  protected val fullSize: Boolean = !canMinimize && !canMaximize

  protected def showMaximize: Boolean =
    sizeState === TileSizeState.Minimized || (canMaximize && sizeState === TileSizeState.Minimized)

  protected def showMinimize: Boolean =
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
  type TileId = NonEmptyString

  private type Props[A] = Tile[A]

  private def componentBuilder[A] =
    ScalaFnComponent
      .withHooks[Props[A]]
      .useStateViewBy(_.initialState)
      .render: (p, sharedState) =>
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

        // Tile wrapper
        if (!p.hidden) {
          <.div(
            ExploreStyles.Tile |+| ExploreStyles.FadeIn |+| p.tileClass,
            ^.key := "tile-${p.id.value}"
          )(
            // Tile title
            <.div(ExploreStyles.TileTitle)(
              ^.key := s"tileTitle-${p.id.value}",
              // Title and optional back button
              <.div(
                ExploreStyles.TileTitleMenu |+| p.tileTitleClass,
                p.back.map(b => <.div(ExploreStyles.TileButton, b)),
                <.div(ExploreStyles.TileTitleText |+| ExploreStyles.TileDraggable, p.title)
              ),
              // Title controls
              <.div(
                ExploreStyles.TileTitleControlArea,
                <.div(ExploreStyles.TileTitleStrip |+| ExploreStyles.TileControl,
                      p.tileTitle(sharedState, p.sizeState)
                )
              ),
              // Size control buttons
              <.div(ExploreStyles.TileControlButtons,
                    minimizeButton.when(p.showMinimize),
                    maximizeButton.when(p.showMaximize)
              ).unless(p.fullSize)
            ),
            // Tile body
            <.div(^.key := s"tileBody-${p.id.value}",
                  ExploreStyles.TileBody |+| p.bodyClass,
                  p.tileBody(sharedState)
            )
              .unless(p.sizeState === TileSizeState.Minimized)
          )
        } else EmptyVdom

  private val component = componentBuilder[Any]
