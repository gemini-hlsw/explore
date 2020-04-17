// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import explore.implicits._
import japgolly.scalajs.react._
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.gridlayout._
import react.sizeme._
import model._
import explore.todo.ToDos
import explore.polls.Polls
import explore.polls.PollsConnectionStatus
import explore.starwars.EpisodeHero

object HomeComponentGQL {
  private val layoutLg: Layout = Layout(
    List(
      LayoutItem(x = 0, y = 0, w  = 6, h = 15, i = "todos"),
      LayoutItem(x = 0, y = 15, w = 6, h = 5, i  = "starwars"),
      LayoutItem(x = 6, y = 0, w  = 6, h = 15, i = "polls"),
      LayoutItem(x = 6, y = 15, w = 6, h = 5, i  = "pollsStatus")
    )
  )

  private val layoutMd: Layout = Layout(
    List(
      LayoutItem(x = 0, y = 0, w  = 5, h = 15, i = "todos"),
      LayoutItem(x = 0, y = 15, w = 5, h = 5, i  = "starwars"),
      LayoutItem(x = 5, y = 0, w  = 5, h = 15, i = "polls"),
      LayoutItem(x = 5, y = 15, w = 5, h = 5, i  = "pollsStatus")
    )
  )

  private val layouts: Map[BreakpointName, (JsNumber, JsNumber, Layout)] =
    Map(
      (BreakpointName.lg, (1200, 12, layoutLg)),
      (BreakpointName.md, (996, 10, layoutMd))
      // (BreakpointName.sm, (768, 8, layout)),
      // (BreakpointName.xs, (480, 6, layout))
    )

  type Props = ViewCtxIO[RootModel]

  private implicit val propsReuse: Reusability[Props] = ViewCtxIOReusability[RootModel]

  protected val component =
    ScalaComponent
      .builder[Props]("HomeGQL")
      .initialState(0)
      .render_P { props =>
        implicit val ctx = props.ctx

        <.div(
          ^.cls := "rgl-area",
          SizeMe() { s =>
            ResponsiveReactGridLayout(
              s.width,
              margin           = (5: JsNumber, 5: JsNumber),
              containerPadding = (5: JsNumber, 5: JsNumber),
              className        = "layout",
              rowHeight        = 30,
              draggableHandle  = ".tileTitle",
              onLayoutChange   = (a, b) => Callback.log(a.toString) *> Callback.log(b.toString),
              layouts          = layouts
            )(
              // ToDos Demo
              <.div(^.key := "todos",
                    ^.cls := "tile",
                    Tile(Tile.Props("ToDos"), ToDos(props.zoomL(RootModel.todoList)))),
              // Starwars Demo
              <.div(^.key := "starwars",
                    ^.cls := "tile",
                    Tile(Tile.Props("Star Wars"), EpisodeHero())),
              // Polls demo
              <.div(^.key := "polls",
                    ^.cls := "tile",
                    Tile(Tile.Props("Polls"), Polls(props.zoomL(RootModel.polls)))),
              <.div(^.key := "pollsStatus",
                    ^.cls := "tile",
                    Tile(Tile.Props("Polls Client Status"), PollsConnectionStatus()))
            )
          }
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build

  def apply(props: Props) = component(props)
}
