// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.Order.*
import cats.*
import cats.syntax.all.*
import clue.FetchClient
import crystal.react.*
import crystal.react.hooks.*
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
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import monocle.Traversal
import queries.schemas.UserPreferencesDB
import react.common.ReactFnProps
import react.common.implicits.given
import react.common.style.Css
import react.gridlayout.*

import scala.concurrent.duration.*
import scala.scalajs.js.JSConverters.*

case class TileController(
  userId:        Option[User.Id],
  gridWidth:     Int,
  defaultLayout: LayoutsMap,
  layoutMap:     LayoutsMap,
  tiles:         List[Tile],
  section:       GridLayoutSection,
  clazz:         Option[Css] = None,
  storeLayout:   Boolean = true
) extends ReactFnProps(TileController.component)

object TileController:
  private type Props = TileController

  private def storeLayouts[F[_]: MonadThrow: Dispatch](
    userId:    Option[User.Id],
    section:   GridLayoutSection,
    layouts:   Layouts,
    debouncer: Reusable[UseSingleEffect[F]]
  )(using FetchClient[F, UserPreferencesDB]): Callback =
    debouncer
      .submit(GridLayouts.storeLayoutsPreference[F](userId, section, layouts))
      .runAsyncAndForget

  // Calculate the state out of the height
  private def unsafeSizeToState(
    layoutsMap: LayoutsMap,
    tileId:     Tile.TileId
  ): TileSizeState = {
    val k = allTiles
      .filter(s => s.i.forall(_ === tileId.value))
      .getAll(layoutsMap)
      .headOption

    val h = k.map(layoutItemHeight.get)
    if (h.exists(_ === 1)) TileSizeState.Minimized else TileSizeState.Normal
  }

  private val allTiles: Traversal[LayoutsMap, LayoutItem] =
    allLayouts.andThen(layoutItems)

  private def unsafeTileHeight(id: Tile.TileId): Traversal[LayoutsMap, Int] =
    allTiles
      .filter(_.i.forall(_ === id.value))
      .andThen(layoutItemHeight)

  private def tileResizable(id: Tile.TileId): Traversal[LayoutsMap, Boolean | Unit] =
    allTiles
      .filter(_.i.forall(_ === id.value))
      .andThen(layoutItemResizable)

  private def updateResizableState(p: LayoutsMap): LayoutsMap =
    allLayouts
      .andThen(layoutItems)
      .modify {
        case r if r.h === 1 => r.copy(isResizable = false, minH = 1)
        case r              => r
      }(p)

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useSingleEffect(debounce = 1.second)
      // Get the breakpoint from the layout
      .useStateBy { (p, _, _) =>
        getBreakpointFromWidth(p.layoutMap.map { case (x, (w, _, _)) => x -> w }, p.gridWidth)
      }
      // Make a local copy of the layout fixing the state of minimized layouts
      .useStateViewBy((p, _, _, _) => updateResizableState(p.layoutMap))
      // Update the current layout if it changes upstream
      .useEffectWithDepsBy((p, _, _, _, _) => p.layoutMap)((_, _, _, _, currentLayout) =>
        layout => currentLayout.set(updateResizableState(layout))
      )
      .render { (p, ctx, debouncer, breakpoint, currentLayout) =>
        import ctx.given

        def sizeState(id: Tile.TileId) = (st: TileSizeState) =>
          currentLayout
            .zoom(allTiles)
            .mod {
              case l if l.i.forall(_ === id.value) =>
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
              case l                               => l
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
          draggableHandle = s".${ExploreStyles.TileTitleMenu.htmlClass}",
          onBreakpointChange = (bk: BreakpointName, _: Int) => breakpoint.setState(bk),
          onLayoutChange = (_: Layout, newLayouts: Layouts) => {
            // We need to sort the layouts to do proper comparision
            val cur     =
              newLayouts.layouts.map(x => (x.name, x.layout)).sortBy(_._1).map { case (n, l) =>
                (n, l.l.sortBy(_.i.toString))
              }
            val next    = currentLayout.get
              .map { case (breakpoint, (_, _, layout)) =>
                (breakpoint, layout.l.sortBy(_.i.toString))
              }
              .toList
              .sortBy(_._1)
            val changed = cur =!= next

            {
              val le = newLayouts.layouts.find(_.name.name === breakpoint.value.name).map(_.layout)

              // Store the current layout in the state for debugging
              le.map(l => currentLayout.mod(breakpointLayout(breakpoint.value).replace(l)))
            }.getOrEmpty *>
              storeLayouts(p.userId, p.section, newLayouts, debouncer)
                .when_(changed && p.storeLayout)
          },
          layouts = currentLayout.get,
          className = p.clazz.map(_.htmlClass).orUndefined
        )(
          p.tiles.map { t =>
            <.div(
              ^.key := t.id.value,
              // Show tile proprties on the title if enabled
              currentLayout.get
                .get(breakpoint.value)
                .flatMap { case (_, _, l) =>
                  l.l
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
              t.controllerClass.orEmpty,
              t.withState(unsafeSizeToState(currentLayout.get, t.id), sizeState(t.id))
            )
          }.toVdomArray
        )
      }
