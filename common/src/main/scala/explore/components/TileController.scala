// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.common.UserPreferencesQueries._
import explore.common.UserPreferencesQueriesGQL._
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.Constants
import explore.model.GridLayoutSection
import explore.model.enum.TileSizeState
import explore.model.layout._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.User
import lucuma.ui.reusability._
import monocle.Traversal
import monocle.unsafe.UnsafeSelect
import react.common._
import react.gridlayout._

import scala.concurrent.duration._

final case class TileController(
  userId:           Option[User.Id],
  coreWidth:        Int,
  defaultLayout:    LayoutsMap,
  layoutMap:        View[LayoutsMap],
  tiles:            List[Tile]
)(implicit val ctx: AppContextIO)
    extends ReactProps[TileController](TileController.component)

object TileController {
  type Props = TileController

  implicit val propsReuse: Reusability[TileController] = Reusability.derive

  def storeLayouts(userId: Option[User.Id], layouts: Layouts)(implicit
    ctx:                   AppContextIO
  ): Callback =
    UserGridLayoutUpsert
      .storeLayoutsPreference[IO](userId, GridLayoutSection.ObservationsLayout, layouts)
      .runAsyncAndForgetCB
      .debounce(1.second)

  val itemHeght = layoutItems.composeLens(layoutItemHeight)

  // Calculate the state out of the height
  def unsafeSizeToState(
    layoutsMap: LayoutsMap,
    tileId:     Tile.TileId
  ): TileSizeState = {
    val k = allTiles
      .composePrism(UnsafeSelect.unsafeSelect(s => s.i.forall(_ === tileId.value)))
      .getAll(layoutsMap)
      .headOption

    val h = k.map(layoutItemHeight.get)
    if (h === Some(1)) TileSizeState.Minimized else TileSizeState.Normal
  }

  val allTiles: Traversal[LayoutsMap, LayoutItem] =
    allLayouts.composeTraversal(layoutItems)

  def unsafeTileHeight(id: Tile.TileId): Traversal[LayoutsMap, Int] =
    allTiles
      .composePrism(UnsafeSelect.unsafeSelect(_.i.forall(_ === id.value)))
      .composeLens(layoutItemHeight)

  val component =
    ScalaComponent
      .builder[Props]
      .stateless
      .render_P { p =>
        def sizeState(id: Tile.TileId, st: TileSizeState): Callback =
          p.layoutMap
            .zoom(allTiles)
            .mod {
              case l if l.i.forall(_ === id.value) =>
                if (st === TileSizeState.Minimized)
                  l.copy(h = 1)
                else if (st === TileSizeState.Normal) {
                  val defaultHeight = unsafeTileHeight(id).headOption(p.defaultLayout).getOrElse(1)
                  // TODO: Restore to the previous size
                  l.copy(h = defaultHeight)
                } else l
              case l                               => l
            }

        ResponsiveReactGridLayout(
          width = p.coreWidth,
          margin = (5, 5),
          containerPadding = (5, 0),
          rowHeight = Constants.GridRowHeight,
          draggableHandle = s".${ExploreStyles.TileTitleMenu.htmlClass}",
          onLayoutChange = (_: Layout, b: Layouts) => storeLayouts(p.userId, b)(p.ctx),
          layouts = p.layoutMap.get
        )(
          p.tiles.map { t =>
            <.div(
              ^.key := t.id.value,
              t.withState(unsafeSizeToState(p.layoutMap.get, t.id), Reuse(sizeState _)(t.id))
            )
          }.toVdomArray
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build

}
