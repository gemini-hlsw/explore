// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.implicits._
import crystal.implicits._
import crystal.react.implicits._
import explore.components.graphql.SubscriptionRenderMod
import explore.components.ui.GPPStyles
import explore.conditions.ConditionsPanel
import explore.conditions.ConditionsQueries._
import explore.implicits._
import explore.model._
import explore.model.reusability._
import explore.target.TargetEditor
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
import explore.observationtree.TargetObsList

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

  type Props = View[RootModel]

  final case class State(treeWidth: JsNumber)

  implicit val stateReuse: Reusability[State] = Reusability.derive

  // BEGIN DEMO PURPOSES - The stream should come from a DB
  import fs2.concurrent.SignallingRef
  import cats.effect.implicits._
  import cats.effect.IO
  import cats.effect.SyncIO
  import crystal.react.StreamRendererMod
  import explore.observationtree.TargetTreeTest

  import AppCtx._

  val TreeComponent =
    AppCtx
      .withCtx { implicit ctx =>
        val obsRef =
          SignallingRef
            .in[SyncIO, IO, List[ExploreObservation]](TargetTreeTest.observations)
            .unsafeRunSync()

        StreamRendererMod.build(obsRef.discrete)
      }

  val targets = TargetTreeTest.targets
  // END DEMO PURPOSES

  class Backend($ : BackendScope[Props, State]) {
    private val targetEditorRef = Ref.toScalaComponent(TargetEditor.component)

    def render(props: Props, state: State) = {
      val obsId = Observation
        .Id(ProgramId.Science.fromString.getOption("GS-2020A-DS-1").get, Index.One)

      val treeResize = (_: ReactEvent, d: ResizeCallbackData) => $.setState(State(d.size.width))

      conditionsSubscription(obsId) { conditions =>
        <.div(
          GPPStyles.RGLArea,
          SizeMe() { s =>
            val treeWidth = state.treeWidth.toDouble
            val coreWidth = s.width.toDouble - treeWidth

            // Tree area
            val tree =
              <.div(^.width := treeWidth.px, GPPStyles.Tree)(
                <.div(GPPStyles.TreeBodyOuter)(
                  TreeComponent(
                    _.map[VdomNode](obsView =>
                      TargetObsList(
                        targets,
                        obsView,
                        props.zoomO(RootModel.focusedTargetOrObsId),
                        targetId =>
                          targets
                            .find(_.id === targetId)
                            .map(target =>
                              targetEditorRef.get
                                .flatMapCB(_.backend.searchTarget(target.name))
                                .toCallback
                            )
                            .getOrEmpty
                      )
                    ).toOption
                      .getOrElse(<.div)
                  )
                )
              )

            <.div(
              GPPStyles.TreeRGL,
              Resizable(
                axis = Axis.X,
                width = treeWidth,
                height = Option(s.height).getOrElse(0),
                minConstraints = (295: JsNumber, 0: JsNumber),
                maxConstraints = (s.width.toDouble / 2: JsNumber, 0: JsNumber),
                onResize = treeResize,
                resizeHandles = List(ResizeHandleAxis.East),
                content = tree
              ),
              <.div(^.width := coreWidth.px, ^.left := treeWidth.px, GPPStyles.RGLBody)(
                ResponsiveReactGridLayout(
                  width = coreWidth,
                  margin = (5: JsNumber, 5: JsNumber),
                  containerPadding = (5: JsNumber, 5: JsNumber),
                  rowHeight = 30,
                  draggableHandle = ".tileTitle",
                  useCSSTransforms =
                    false, // Not ideal, but fixes flicker on first update (0.18.3).
                  // onLayoutChange = (a, b) => Callback.log(a.toString) *> Callback.log(b.toString),
                  layouts = layouts
                )(
                  <.div(
                    ^.key := "conditions",
                    ^.cls := "tile",
                    Tile("Conditions")(
                      ConditionsPanel(obsId, conditions)
                    )
                  ),
                  <.div(
                    ^.key := "target",
                    ^.cls := "tile",
                    Tile("Target Position")(
                      TargetEditor(obsId, /*props.zoomL(RootModel.target),*/ conditions.get.some)
                        .withRef(targetEditorRef)
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
