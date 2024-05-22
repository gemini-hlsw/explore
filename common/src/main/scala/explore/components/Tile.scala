// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.components.ui.ExploreStyles.*
import explore.model.enums.TileSizeState
import explore.model.layout
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import lucuma.react.common.style.*
import lucuma.react.primereact.Button
import lucuma.ui.syntax.all.given
import org.scalajs.dom

case class Tile(
  id:                 Tile.TileId,
  title:              String,
  back:               Option[VdomNode] = None,
  control:            TileSizeState => Option[VdomNode] = _ => None,
  canMinimize:        Boolean = false,
  canMaximize:        Boolean = false,
  hidden:             Boolean = false,
  state:              TileSizeState = TileSizeState.Maximized,
  sizeStateCallback:  TileSizeState => Callback = _ => Callback.empty,
  controllerClass:    Css = Css.Empty, // applied to wrapping div when in a TileController.
  bodyClass:          Css = Css.Empty, // applied to tile body
  tileClass:          Css = Css.Empty, // applied to the tile
  tileTitleClass:     Css = Css.Empty, // applied to the title
  renderInTitleClass: Css = Css.Empty  // applied to the portal in the title
)(val render: Tile.RenderInTitle => VdomNode)
    extends ReactFnProps[Tile](Tile.component) {
  def showMaximize: Boolean =
    state === TileSizeState.Minimized || (canMaximize && state === TileSizeState.Minimized)

  def showMinimize: Boolean =
    state === TileSizeState.Maximized || (canMinimize && state === TileSizeState.Maximized)

  def withState(
    state:             TileSizeState,
    sizeStateCallback: TileSizeState => Callback
  ): Tile =
    copy(state = state, sizeStateCallback = sizeStateCallback)(render)
}

object Tile:
  type TileId        = NonEmptyString
  type RenderInTitle = VdomNode => VdomNode

  private type Props = Tile

  private val heightBreakpoints =
    List((200, TileXSH), 700 -> TileSMH, 1024 -> TileMDH)

  private val widthBreakpoints =
    List(layout.XtraSmallCutoff -> TileXSW,
         layout.SmallCutoff     -> TileSMW,
         layout.MediumCutoff    -> TileMDW,
         layout.LargeCutoff     -> TileLGW
    )

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      // infoRef - We use state instead of a regular Ref in order to force a rerender when it's set.
      .useState(none[dom.html.Element])
      .render: (p, infoRef) =>
        val maximizeButton =
          Button(
            text = true,
            clazz = ExploreStyles.TileStateButton,
            icon = Icons.Maximize,
            onClick = p
              .sizeStateCallback(TileSizeState.Maximized)
              .when_(p.state === TileSizeState.Minimized)
          )

        val minimizeButton =
          Button(
            text = true,
            clazz = ExploreStyles.TileStateButton,
            icon = Icons.Minimize,
            onClick = p
              .sizeStateCallback(TileSizeState.Minimized)
              .when_(p.state === TileSizeState.Maximized)
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
            // Tile title, set classes based on size
            ResponsiveComponent(
              widthBreakpoints,
              heightBreakpoints,
              clazz = ExploreStyles.TileTitle |+| p.tileTitleClass
            )(
              React.Fragment(
                <.div(
                  ExploreStyles.TileTitleMenu,
                  p.back.map(b => <.div(ExploreStyles.TileButton, b)),
                  <.span(ExploreStyles.TileTitleControlArea, p.title)
                ),
                <.div(
                  p.control(p.state)
                    .map(b => <.div(ExploreStyles.TileControl, b)),
                  <.div(^.key := "tileTitle", ^.untypedRef(setInfoRef).when(infoRef.value.isEmpty))(
                    ExploreStyles.TileTitleStrip |+| p.renderInTitleClass,
                    ExploreStyles.FixedSizeTileTitle.when(!p.canMinimize && !p.canMaximize)
                  )
                ),
                <.div(ExploreStyles.TileControlButtons,
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
                  clazz = ExploreStyles.TileBody |+| p.bodyClass
                )(
                  p.render(info => ReactPortal(info, node))
                ).when(p.state =!= TileSizeState.Minimized)
              )
              .whenDefined
          )
        } else EmptyVdom
