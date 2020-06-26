// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.implicits._
import crystal.implicits._
import crystal.react.implicits._
import explore.components.graphql.SubscriptionRenderMod
import explore.components.ui.GPPStyles
import explore.constraints.ConstraintsPanel
import explore.constraints.ConstraintsQueries._
import explore.implicits._
import explore.model._
import explore.model.reusability._
import explore.observationtree.TargetObsList
import explore.observationtree.TargetObsQueries._
import explore.targeteditor.TargetEditor
import gem.Observation
import gem.ProgramId
import gem.util.Enumerated
import gsp.math.Index
import japgolly.scalajs.react._
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.vdom.html_<^._
import monocle.function.Cons.headOption
import react.common._
import react.draggable.Axis
import react.gridlayout._
import react.resizable._
import react.sizeme._
import java.util.UUID

object HomeComponent {
  private val layoutLg: Layout = Layout(
    List(
      LayoutItem(x = 0, y = 0, w = 12, h = 16, i = "target"),
      LayoutItem(x = 0, y = 8, w = 12, h = 7, i = "constraints")
    )
  )

  private val layoutMd: Layout = Layout(
    List(
      LayoutItem(x = 0, y = 0, w = 12, h = 16, i = "target"),
      LayoutItem(x = 0, y = 8, w = 12, h = 7, i = "constraints")
    )
  )

  private val layouts: Map[BreakpointName, (JsNumber, JsNumber, Layout)] =
    Map(
      (BreakpointName.lg, (1200, 12, layoutLg)),
      (BreakpointName.md, (996, 10, layoutMd))
      // (BreakpointName.sm, (768, 8, layout)),
      // (BreakpointName.xs, (480, 6, layout))
    )

  type Props = View[RootModel]

  final case class State(treeWidth: JsNumber)

  implicit val stateReuse: Reusability[State] = Reusability.derive

  class Backend($ : BackendScope[Props, State]) {
    private val targetEditorRef = Ref.toScalaComponent(TargetEditor.component)

    def render(props: Props, state: State) = {
      val constraintsId = UUID.fromString("608c8407-63a5-4d26-970c-587486af57da")
      val targetId      = UUID.fromString("9be5789c-3ffe-48cd-8e8e-24fe3e4067ee")

      val treeResize = (_: ReactEvent, d: ResizeCallbackData) => $.setState(State(d.size.width))

      constraintsSubscription(constraintsId) { constraints =>
        <.div(
          GPPStyles.RGLArea,
          SizeMe() { s =>
            val treeWidth = state.treeWidth.toDouble
            val coreWidth = s.width.toDouble - treeWidth

            // Tree area
            val tree =
              <.div(^.width := treeWidth.px, GPPStyles.Tree)(
                <.div(GPPStyles.TreeBodyOuter)(
                  <.div(GPPStyles.TreeBodyInner)(
                    targetObsSubscription((targets, obsView) =>
                      <.div(
                        TargetObsList(
                          targets,
                          obsView,
                          props.zoom(RootModel.focusedTargetOrObsId),
                          _ => Callback.empty
                        )
                      )
                    )
                  )
                )
              )

            <.div(
              GPPStyles.TreeRGL,
              Resizable(
                axis = Axis.X,
                width = treeWidth,
                height = Option(s.height).getOrElse(0),
                minConstraints = (250, 0),
                maxConstraints = (s.width.toInt / 2, 0),
                onResize = treeResize,
                resizeHandles = List(ResizeHandleAxis.East),
                content = tree
              ),
              <.div(^.width := coreWidth.px, ^.left := treeWidth.px, GPPStyles.RGLBody)(
                ResponsiveReactGridLayout(
                  width = coreWidth,
                  margin = (5, 5),
                  containerPadding = (5, 5),
                  rowHeight = 30,
                  draggableHandle = ".tileTitle",
                  useCSSTransforms =
                    false, // Not ideal, but fixes flicker on first update (0.18.3).
                  // onLayoutChange = (a, b) => Callback.log(a.toString) *> Callback.log(b.toString),
                  layouts = layouts
                )(
                  <.div(
                    ^.key := "constraints",
                    ^.cls := "tile",
                    Tile("Constraints")(
                      ConstraintsPanel(constraintsId, constraints)
                    )
                  ),
                  <.div(
                    ^.key := "target",
                    ^.cls := "tile",
                    Tile("Target Position")(
                      TargetEditor(targetId, constraints.get.some).withRef(targetEditorRef)
                    )
                  )
                )
              )
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
