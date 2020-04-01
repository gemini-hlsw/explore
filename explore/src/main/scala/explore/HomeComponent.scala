// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import explore.conditions.ConditionsPanel
import japgolly.scalajs.react._
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.vdom.html_<^._
import react.semanticui.elements.button._
import react.common._
import react.gridlayout._
import react.sizeme._
import model._
import explore.todo.ToDos
import explore.polls.Polls
import explore.model.AppStateIO._
import crystal.react.StreamRenderer
import crystal.react.io.implicits._
import gem.Observation
import gem.ProgramId
import gsp.math.Index

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

  private val pollConnectionStatus =
    StreamRenderer.build(AppState.clients.polls.statusStream, Reusability.derive)

  val component =
    ScalaComponent
      .builder[RootModel]("Home")
      .initialState(0)
      .renderPS { (_, p, _) =>
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
              <.div(^.key := "tpe",    ^.cls := "tile", Tile(Tile.Props("Conditions"), 
                ConditionsPanel(Observation.Id(ProgramId.Science.fromString.getOption("Program").get, Index.One))
              )),
              <.div(^.key := "coords", ^.cls := "tile", Tile(Tile.Props("Coordinates"), Imag())),
              <.div(
                ^.key := "doc",
                ^.cls := "tile",
                Tile(
                  Tile.Props("Target Position"),
                  <.span(
                    pollConnectionStatus(status => <.div(s"Poll connection is: $status")),
                    Button(onClick = AppState.clients.polls.close())("Close Connection")
                  ),
                  Polls(p.polls),
                  ToDos(p.todoList),
                  <.div(p.target.whenDefined(Tpe(_)))
                )
              )
            )
          }
        )
      }
      .configure(Reusability.shouldComponentUpdate)
      .build

  def apply(model: RootModel) = component(model)
}
