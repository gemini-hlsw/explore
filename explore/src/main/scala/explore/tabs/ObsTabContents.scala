// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import scala.annotation.unused

import cats.syntax.all._
import crystal.react.implicits._
import explore._
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.Focused.FocusedObs
import explore.model._
import explore.model.reusability._
import explore.observationtree.ObsList
import explore.observationtree.ObsQueries._
import japgolly.scalajs.react._
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.vdom.html_<^._
import react.common._
import react.draggable.Axis
import react.gridlayout._
import react.resizable._
import react.sizeme._

object ObsTabContents {
  private val layoutLg: Layout = Layout(
    List(
      LayoutItem(x = 0, y = 0, w = 12, h = 16, i = "target"),
      LayoutItem(x = 0, y = 8, w = 12, h = 8, i = "constraints")
    )
  )

  private val layoutMd: Layout = Layout(
    List(
      LayoutItem(x = 0, y = 0, w = 12, h = 16, i = "target"),
      LayoutItem(x = 0, y = 8, w = 12, h = 8, i = "constraints")
    )
  )

  private val layouts: Map[BreakpointName, (JsNumber, JsNumber, Layout)] =
    Map(
      (BreakpointName.lg, (1200, 12, layoutLg)),
      (BreakpointName.md, (996, 10, layoutMd))
      // (BreakpointName.sm, (768, 8, layout)),
      // (BreakpointName.xs, (480, 6, layout))
    )

  type Props = View[Option[Focused]]

  final case class State(treeWidth: JsNumber)

  implicit val stateReuse: Reusability[State] = Reusability.derive

  class Backend($ : BackendScope[Props, State]) {
    def render(props: Props, state: State) = {
      val treeResize = (_: ReactEvent, d: ResizeCallbackData) => $.setState(State(d.size.width))
      val treeWidth  = state.treeWidth.toDouble

      // Tree area
      def tree(observations: View[List[ObsSummary]]) =
        <.div(^.width := treeWidth.px, ExploreStyles.Tree)(
          <.div(ExploreStyles.TreeBody)(
            ObsList(
              observations,
              props
            )
          )
        )

      ObsLiveQuery { observations =>
        @unused val obsSummaryOpt = props.get.collect { case FocusedObs(obsId) =>
          observations.get.find(_.id === obsId)
        }.flatten

        <.div(
          ExploreStyles.RGLArea,
          SizeMe() { s =>
            val coreWidth = s.width.toDouble - treeWidth

            <.div(
              ExploreStyles.TreeRGL,
              Resizable(
                axis = Axis.X,
                width = treeWidth,
                height = Option(s.height).getOrElse(0),
                minConstraints = (270, 0),
                maxConstraints = (s.width.toInt / 2, 0),
                onResize = treeResize,
                resizeHandles = List(ResizeHandleAxis.East),
                content = tree(observations)
              ),
              <.div(^.width := coreWidth.px, ^.left := treeWidth.px, ExploreStyles.RGLBody)(
                ResponsiveReactGridLayout(
                  width = coreWidth,
                  margin = (5, 5),
                  containerPadding = (5, 5),
                  rowHeight = 30,
                  draggableHandle = s".${ExploreStyles.TileTitle.htmlClass}",
                  // onLayoutChange = (a, b) => Callback.log(a.toString) *> Callback.log(b.toString),
                  layouts = layouts
                )(
                  <.div(
                    ^.key := "target",
                    ^.cls := "tile",
                    Tile("Target Position", movable = true, None)(
                      <.span(
                        // obsSummaryOpt.whenDefined(obs =>
                        //   TargetEditor(obs.target.id).withKey(obs.target.id.toString)
                        // )
                      )
                    )
                  ),
                  <.div(
                    ^.key := "constraints",
                    ^.cls := "tile",
                    Tile("Constraints", movable = true, None)(
                      <.span(
                        // obsSummaryOpt.whenDefined(obs =>
                        //   ConstraintsSubscription(obs.constraints.id) { constraints =>
                        //     ConstraintsPanel(obs.constraints.id, constraints)
                        //   }.withKey(obs.constraints.id.toString)
                        // )
                      )
                    )
                  )
                )
              ).when(false),
              <.div(^.width := coreWidth.px, ^.left := treeWidth.px, ExploreStyles.RGLPlaceholder)
            )
          }
        )
      }
    }
  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .initialState(State(295))
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build

  def apply(props: Props) = component(props)
}
