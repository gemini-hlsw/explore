// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import java.util.UUID

import cats.implicits._
import crystal.react.implicits._
import explore._
import explore.components.ui.GPPStyles
import explore.constraints.ConstraintsPanel
import explore.constraints.ConstraintsQueries._
import explore.model.Focused.FocusedObs
import explore.model.Focused.FocusedTarget
import explore.model._
import explore.model.reusability._
import explore.observationtree.TargetObsList
import explore.observationtree.TargetObsQueries._
import explore.targeteditor.TargetEditor
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

  type Props = View[Option[Focused]]

  final case class State(treeWidth: JsNumber)

  implicit val stateReuse: Reusability[State] = Reusability.derive

  class Backend($ : BackendScope[Props, State]) {
    def render(props: Props, state: State) = {
      val constraintsId = UUID.fromString("608c8407-63a5-4d26-970c-587486af57da")

      val treeResize = (_: ReactEvent, d: ResizeCallbackData) => $.setState(State(d.size.width))
      val treeWidth  = state.treeWidth.toDouble

      // Tree area
      def tree(targetsWithObs: View[TargetsWithObs]) =
        <.div(^.width := treeWidth.px, GPPStyles.Tree)(
          <.div(GPPStyles.TreeBodyOuter)(
            <.div(GPPStyles.TreeBodyInner)(
              TargetObsList(
                targetsWithObs,
                props
              )
            )
          )
        )

      TargetObsSubscription { targetsWithObs =>
        val targetIdOpt = props.get.collect {
          case FocusedTarget(targetId) => targetId.some
          case FocusedObs(obsId)       => targetsWithObs.get.obs.getElement(obsId).map(_.target.id)
        }.flatten

        <.div(
          GPPStyles.RGLArea,
          SizeMe() { s =>
            val coreWidth = s.width.toDouble - treeWidth

            <.div(
              GPPStyles.TreeRGL,
              Resizable(
                axis = Axis.X,
                width = treeWidth,
                height = Option(s.height).getOrElse(0),
                minConstraints = (270, 0),
                maxConstraints = (s.width.toInt / 2, 0),
                onResize = treeResize,
                resizeHandles = List(ResizeHandleAxis.East),
                content = tree(targetsWithObs)
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
                    ^.key := "target",
                    ^.cls := "tile",
                    Tile("Target Position")(
                      <.span(
                        targetIdOpt.whenDefined(targetId =>
                          TargetEditor(targetId)
                            .withKey(targetId.toString)
                        )
                      )
                    )
                  ),
                  <.div(
                    ^.key := "constraints",
                    ^.cls := "tile",
                    Tile("Constraints")(
                      ConstraintsSubscription(constraintsId) { constraints =>
                        ConstraintsPanel(constraintsId, constraints)
                      }
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
