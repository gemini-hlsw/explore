// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import crystal.implicits._
import explore.conditions.ConditionsPanel
import explore.implicits._
import explore.target.TargetEditor
import gem.Observation
import gem.ProgramId
import gsp.math.Index
import japgolly.scalajs.react._
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.gridlayout._
import react.sizeme._

import model._

object HomeComponent {
  private val layoutLg: Layout = Layout(
    List(
      LayoutItem(x = 0, y = 0, w = 12, h = 16, i = "target"),
      LayoutItem(x = 0, y = 8, w = 12, h = 7, i = "conditions")
    )
  )

  private val layoutMd: Layout = Layout(
    List(
      LayoutItem(x = 0, y = 0, w = 12, h = 16, i = "target"),
      LayoutItem(x = 0, y = 8, w = 12, h = 7, i = "conditions")
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
      .builder[Props]("Home")
      .initialState(0)
      .render_P { props =>
        props.withCtx { implicit ctx =>
          val obsId = Observation
            .Id(ProgramId.Science.fromString.getOption("GS-2020A-DS-1").get, Index.One)
            .inCtx(ctx)
          <.div(
            ^.cls := "rgl-area",
            SizeMe() { s =>
              ResponsiveReactGridLayout(
                s.width,
                margin = (5: JsNumber, 5: JsNumber),
                containerPadding = (5: JsNumber, 5: JsNumber),
                className = "layout",
                rowHeight = 30,
                draggableHandle = ".tileTitle",
                onLayoutChange = (a, b) => Callback.log(a.toString) *> Callback.log(b.toString),
                layouts = layouts
              )(
                <.div(
                  ^.key := "conditions",
                  ^.cls := "tile",
                  Tile("Conditions")(
                    ConditionsPanel(obsId)
                  )
                ),
                <.div(
                  ^.key := "target",
                  ^.cls := "tile",
                  Tile("Target Position")(
                    TargetEditor(obsId)
                  )
                )
              )
            }
          )
        }
      }
      .configure(Reusability.shouldComponentUpdate)
      .build

  def apply(props: Props) = component(props)
}
