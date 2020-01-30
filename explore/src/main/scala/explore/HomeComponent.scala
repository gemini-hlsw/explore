// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import explore.conditions.Conditions
import japgolly.scalajs.react._
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.vdom.html_<^._
import react.semanticui.elements.button._
import react.common._
import react.gridlayout._
import react.sizeme._
import model._
import explore.todo.Todo
import explore.polls.Polls
import cats.effect.IO
import crystal.react.StreamRenderer
import crystal.react.io.implicits._

object HomeComponent {

  private val layoutLg: Layout = Layout(
    List(
      LayoutItem(x = 0, y = 0, w  = 6, h  = 9, i = "tpe"),
      LayoutItem(x = 6, y = 0, w  = 6, h  = 9, i = "coords"),
      LayoutItem(x = 0, y = 10, w = 12, h = 8, i = "doc", isDraggable = false)
    )
  )

  private val layoutMd: Layout = Layout(
    List(
      LayoutItem(x = 0, y = 0, w = 5, h  = 5, i = "tpe"),
      LayoutItem(x = 6, y = 0, w = 5, h  = 5, i = "coords"),
      LayoutItem(x = 0, y = 6, w = 10, h = 6, i = "doc", isDraggable = false)
    )
  )

  private val layouts: Map[BreakpointName, (JsNumber, JsNumber, Layout)] =
    Map(
      (BreakpointName.lg, (1200, 12, layoutLg)),
      (BreakpointName.md, (996, 10, layoutMd))
      // (BreakpointName.sm, (768, 8, layout)),
      // (BreakpointName.xs, (480, 6, layout))
    )

  import AppState._
  private val pollConnectionStatus = 
    StreamRenderer.build(pollClient.statusStream[IO], Reusability.derive)

  val component =
    ScalaComponent
      .builder[Unit]("Home")
      .initialState(0)
      .renderPS { (_, _, _) =>
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
              <.div(^.key := "tpe",    ^.cls := "tile", Tile(Tile.Props("Conditions"), Conditions())),
              <.div(^.key := "coords", ^.cls := "tile", Tile(Tile.Props("Coordinates"), Imag())),
              <.div(
                ^.key := "doc",
                ^.cls := "tile",
                Tile(
                  Tile.Props("Target Position"),
                  Todo(Views.todoList),
                  <.span(
                    pollConnectionStatus(status => <.div(s"Poll connection is: $status")),
                    Button(onClick = pollClient.close[IO]())("Close Connection")
                  ),
                  Views.polls.streamRender(Polls.apply),
                  Views.target
                    .streamRender(targetOpt => <.div(targetOpt.whenDefined(target => Tpe(target))))
                )
              )
            )
          }
        )
      }
      .build

  def apply() = component()
}
