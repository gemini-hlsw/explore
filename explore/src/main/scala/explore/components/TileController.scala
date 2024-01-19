// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.Order.*
import cats.*
import cats.syntax.all.*
import clue.FetchClient
import crystal.react.*
import crystal.react.hooks.*
import explore.common.UserPreferencesQueries
import explore.common.UserPreferencesQueries.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Constants
import explore.model.enums.GridLayoutSection
import explore.model.enums.TileSizeState
import explore.model.layout.*
import explore.model.layout.given
import japgolly.scalajs.react.*
import japgolly.scalajs.react.util.Effect.Dispatch
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.User
import lucuma.react.common.ReactFnProps
import lucuma.react.common.style.Css
import lucuma.react.gridlayout.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import monocle.Traversal
import queries.schemas.UserPreferencesDB

import scala.scalajs.js.JSConverters.*

case class TileController(
  userId:        Option[User.Id],
  gridWidth:     Int,
  defaultLayout: LayoutsMap,
  layoutMap:     LayoutsMap,
  tiles:         List[Tile],
  section:       GridLayoutSection,
  backButton:    Option[VdomNode] = None,
  clazz:         Option[Css] = None,
  storeLayout:   Boolean = true
) extends ReactFnProps(TileController.component)

object TileController:
  private type Props = TileController

  private def storeLayouts[F[_]: MonadThrow: Dispatch](
    userId:  Option[User.Id],
    section: GridLayoutSection,
    layouts: Layouts
  )(using FetchClient[F, UserPreferencesDB]): Callback =
    GridLayouts.storeLayoutsPreference[F](userId, section, layouts).runAsyncAndForget

  // Calculate the state out of the height
  private def unsafeSizeToState(
    layoutsMap: LayoutsMap,
    tileId:     Tile.TileId
  ): TileSizeState = {
    val k = allTiles
      .filter(s => s.i === tileId.value)
      .getAll(layoutsMap)
      .headOption

    val h = k.map(layoutItemHeight.get)
    if (h.exists(_ === 1)) TileSizeState.Minimized else TileSizeState.Normal
  }

  private val allTiles: Traversal[LayoutsMap, LayoutItem] =
    allLayouts.andThen(layoutItems)

  private def unsafeTileHeight(id: Tile.TileId): Traversal[LayoutsMap, Int] =
    allTiles
      .filter(_.i === id.value)
      .andThen(layoutItemHeight)

  private def tileResizable(id: Tile.TileId): Traversal[LayoutsMap, Boolean | Unit] =
    allTiles
      .filter(_.i === id.value)
      .andThen(layoutItemResizable)

  private def updateResizableState(tiles: List[Tile], p: LayoutsMap): LayoutsMap =
    allLayouts
      .andThen(layoutItems)
      .modify {
        case r if tiles.exists(t => t.id.value === r.i && t.hidden) =>
          // height to 0 for hidden tiles
          r.copy(isResizable = false, minH = 0, h = 0)
        case r if r.h === 1                                         => r.copy(isResizable = false, minH = 1)
        case r                                                      => r
      }(p)

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // Get the breakpoint from the layout
      .useStateBy { (p, _) =>
        getBreakpointFromWidth(p.layoutMap.map { case (x, (w, _, _)) => x -> w }, p.gridWidth)
      }
      // Make a local copy of the layout fixing the state of minimized layouts
      .useStateViewBy((p, _, _) => updateResizableState(p.tiles, p.layoutMap))
      // resizing
      .useState(TileResizing(false))
      // Update the current layout if it changes upstream
      .useEffectWithDepsBy((p, _, _, _, _) => (p.tiles.map(_.hidden), p.layoutMap))(
        (p, _, _, currentLayout, resizing) =>
          (_, layout) =>
            resizing.setState(TileResizing(false)) *>
              currentLayout.set(updateResizableState(p.tiles, layout))
      )
      .render { (p, ctx, breakpoint, currentLayout, resizing) =>
        import ctx.given

        def sizeState(id: Tile.TileId) = (st: TileSizeState) =>
          resizing.setState(TileResizing(true)) *>
            currentLayout
              .zoom(allTiles)
              .mod {
                case l if l.i === id.value =>
                  if (st === TileSizeState.Minimized) l.copy(h = 1, minH = 1, isResizable = false)
                  else if (st === TileSizeState.Normal) {
                    val defaultHeight =
                      unsafeTileHeight(id).headOption(p.defaultLayout).getOrElse(1)
                    // restore the resizable state
                    val resizable     =
                      tileResizable(id).headOption(p.defaultLayout).getOrElse(true: Boolean | Unit)
                    // TODO: Restore to the previous size
                    l.copy(h = defaultHeight,
                           isResizable = resizable,
                           minH = scala.math.max(l.minH.getOrElse(1), defaultHeight)
                    )
                  } else l
                case l                     => l
              } *> resizing.setState(TileResizing(false)).when(p.storeLayout === false).void

        def addBackButton: List[Tile] = {
          val topTile =
            currentLayout.get.get(breakpoint.value).flatMap(_._3.asList.sortBy(_.y).headOption)
          (topTile, p.backButton)
            .mapN((t, b) =>
              p.tiles
                .map {
                  case ti if t.i === ti.id.value =>
                    ti.copy(back = p.backButton)(ti.render)
                  case ti                        => ti
                }
            )
            .getOrElse(p.tiles)
        }

        ResponsiveReactGridLayout(
          width = p.gridWidth.toDouble,
          autoSize = true,
          // this has a performance cost but lets controls on the title to work properly
          // https://github.com/react-grid-layout/react-grid-layout/issues/858#issuecomment-426346399
          useCSSTransforms = false, // this has a performanco cost but see
          margin = (Constants.GridRowPadding, Constants.GridRowPadding),
          containerPadding = (Constants.GridRowPadding, 0),
          rowHeight = Constants.GridRowHeight,
          draggableHandle = s".${ExploreStyles.TileTitleControlArea.htmlClass}",
          onBreakpointChange = (bk: BreakpointName, _: Int) => breakpoint.setState(bk),
          onLayoutChange = (m: Layout, newLayouts: Layouts) =>
            // Store the current layout in the state for debugging
            currentLayout
              .mod(breakpointLayout(breakpoint.value).replace(m)) *>
              storeLayouts(p.userId, p.section, newLayouts)
                .when_(p.storeLayout),
          layouts = currentLayout.get,
          className = p.clazz.map(_.htmlClass).orUndefined
        )(
          addBackButton.map { t =>
            <.div(
              ^.key := t.id.value,
              // Show tile properties on the title if enabled
              currentLayout.get
                .get(breakpoint.value)
                .flatMap { case (_, _, l) =>
                  l.asList
                    .find(_.i === t.id.value)
                    .flatMap { i =>
                      TagMod
                        .devOnly(
                          <.div(
                            ^.cls := "rgl-tile-overlay",
                            s"id: ${i.i} st: ${t.state} x: ${i.x} y: ${i.y} w: ${i.w} h: ${i.h}${i.minH.toOption
                                .foldMap(m => s" minH: $m")}${i.maxH.toOption
                                .foldMap(m => s" maxH: $m")}${i.minW.toOption
                                .foldMap(m => s" minW: $m")}${i.maxW.toOption
                                .foldMap(m => s" maxW: $m")}${i.isResizable.toOption
                                .foldMap(m => s" isResizable: $m")}"
                          )
                        )
                        .some
                    }
                }
                .getOrElse(EmptyVdom),
              t.controllerClass,
              t.withState(unsafeSizeToState(currentLayout.get, t.id),
                          resizing.value,
                          sizeState(t.id)
              )
            )
          }.toVdomArray
        )
      }
