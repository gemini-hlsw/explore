// Copyright (c) 2016-2019 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import explore.conditions.Conditions
import japgolly.scalajs.react._
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.vdom.html_<^._
import react.gridlayout._
import react.sizeme._
import model._
import explore.todo.Todo

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
                Tile(Tile.Props("Target Position"),
                     Todo(Views.todoList),
                     Views.target.flow(targetOpt => <.div(targetOpt.whenDefined(target => Tpe(target)))))
              )
            )
          }
        )
      }
      .build

  def apply() = component()
}
