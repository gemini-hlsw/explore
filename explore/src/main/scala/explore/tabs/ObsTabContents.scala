// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.kernel.Order
import cats.syntax.all._
import crystal.react.implicits._
import explore._
import explore.components.ui.ExploreStyles
import explore.components.{ Tile, TileButton }
import explore.model.Focused.FocusedObs
import explore.model._
import explore.model.enum.{ AppTab, TileSizeState }
import explore.model.reusability._
import explore.observationtree.ObsList
import explore.observationtree.ObsQueries._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.reusability._
import lucuma.ui.utils._
import monocle.function.Field3._
import monocle.function.Index._
import monocle.macros.{ GenLens, Lenses }
import org.scalajs.dom.window
import react.common._
import react.common.implicits._
import react.draggable.Axis
import react.gridlayout._
import react.resizable._
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.Button.ButtonProps
import react.semanticui.sizes._
import react.sizeme._

import scala.annotation.unused
import scala.collection.immutable.SortedMap

final case class ObsTabContents(
  focused: View[Option[Focused]]
) extends ReactProps[ObsTabContents](ObsTabContents.component) {
  def isObsSelected: Boolean = focused.get.isDefined
}

object ObsTabContents {
  private val layoutLarge: Layout = Layout(
    List(
      LayoutItem(x = 0,
                 y = 0,
                 w = 12,
                 h = 3,
                 i = "notes",
                 isResizable = false,
                 resizeHandles = List("")
      ),
      LayoutItem(x = 0, y = 3, w = 12, h = 8, i = "target")
    )
  )

  private val layoutMedium: Layout = Layout(
    List(
      LayoutItem(x = 0,
                 y = 0,
                 w = 12,
                 h = 3,
                 i = "notes",
                 isResizable = false,
                 resizeHandles = List("")
      ),
      LayoutItem(x = 0, y = 3, w = 12, h = 8, i = "target")
    )
  )

  private val layoutSmall: Layout = Layout(
    List(
      LayoutItem(x = 0,
                 y = 0,
                 w = 12,
                 h = 3,
                 i = "notes",
                 isResizable = false,
                 resizeHandles = List("")
      ),
      LayoutItem(x = 0, y = 3, w = 12, h = 8, i = "target")
    )
  )

  implicit val breakpointNameOrder: Order[BreakpointName] = Order.by(_.name)
  implicit val breakpointNameOrdering                     = breakpointNameOrder.toOrdering

  private val layouts: SortedMap[BreakpointName, (JsNumber, JsNumber, Layout)] =
    SortedMap(
      (BreakpointName.lg, (1200, 12, layoutLarge)),
      (BreakpointName.md, (996, 10, layoutMedium)),
      (BreakpointName.sm, (768, 8, layoutSmall))
    )

  @Lenses
  final case class State(
    panels:  TwoPanelState,
    layouts: SortedMap[BreakpointName, (JsNumber, JsNumber, Layout)]
  )

  object State {
    val panelsWidth      = State.panels.composeLens(TwoPanelState.treeWidth)
    val panelSelected    = State.panels.composeLens(TwoPanelState.elementSelected)
    val layoutLens       = GenLens[Layout](_.l)
    val layoutItemHeight = GenLens[LayoutItem](_.h)

    def breakPoint(n:       BreakpointName) = layouts.composeOptional(index(n))
    def breakPointLayout(n: BreakpointName) = breakPoint(n).composeLens(third)
    def breakPointNote(n:   BreakpointName) =
      breakPointLayout(n).composeLens(layoutLens).composeOptional(index(0))
    def notesHeight =
      breakPointNote(BreakpointName.sm).composeLens(layoutItemHeight)
    def notesHeightState(s: State): TileSizeState = notesHeight.getOption(s) match {
      case Some(x) if x === 1 => TileSizeState.Minimized
      case _                  => TileSizeState.Normal
    }
  }

  type Props = ObsTabContents
  implicit val propsReuse: Reusability[Props] = Reusability.derive
  implicit val stateReuse: Reusability[State] = Reusability.never

  class Backend($ : BackendScope[Props, State]) {
    def render(props: Props, state: State) = {
      AppCtx.withCtx { ctx =>
        val treeResize =
          (_: ReactEvent, d: ResizeCallbackData) => $.setStateL(State.panelsWidth)(d.size.width)
        val treeWidth  = state.panels.treeWidth.toDouble

        // Tree area
        def tree(observations: View[List[ObsSummary]]) =
          <.div(^.width := treeWidth.px, ExploreStyles.Tree |+| ExploreStyles.ResizableMultiPanel)(
            treeInner(observations)
          )

        def treeInner(observations: View[List[ObsSummary]]) =
          <.div(ExploreStyles.TreeBody)(
            ObsList(
              observations,
              props.focused
            )
          )

        ObsLiveQuery { observations =>
          @unused val obsSummaryOpt = props.focused.get.collect { case FocusedObs(obsId) =>
            observations.get.find(_.id === obsId)
          }.flatten

          val backButton = TileButton(
            Button(
              as = <.a,
              basic = true,
              size = Mini,
              compact = true,
              clazz = ExploreStyles.TileBackButton |+| ExploreStyles.BlendedButton,
              onClickE = linkOverride[IO, ButtonProps](props.focused.set(none))
            )(^.href := ctx.pageUrl(AppTab.Observations, none), Icons.ChevronLeft.fitted(true))
          )

          React.Fragment(
            SizeMe() { s =>
              val coreWidth =
                if (window.innerWidth <= Constants.TwoPanelCutoff) {
                  s.width.toDouble
                } else {
                  s.width.toDouble - treeWidth
                }
              val rightSide = ResponsiveReactGridLayout(
                width = coreWidth,
                margin = (5, 5),
                containerPadding = (5, 0),
                rowHeight = Constants.GridRowHeight,
                draggableHandle = s".${ExploreStyles.TileTitleMenu.htmlClass}",
                // onLayoutChange = (a, b) => Callback.log(a.toString) *> Callback.log(b.toString),
                layouts = state.layouts
              )(
                <.div(
                  ^.key := "notes",
                  Tile(
                    "Note for Observer",
                    backButton.some,
                    canMinimize = true,
                    canMaximize = true,
                    state = State.notesHeightState(state),
                    sizeStateCallback = (s: TileSizeState) =>
                      $.setStateL(State.notesHeight)(s match {
                        case TileSizeState.Minimized => 1
                        case _                       => 3
                      })
                  )(
                    <.div(
                      ExploreStyles.NotesWrapper,
                      <.div(
                        ExploreStyles.ObserverNotes,
                        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus maximus hendrerit lacinia. Etiam dapibus blandit ipsum sed rhoncus."
                      )
                    )
                  )
                ),
                <.div(
                  ^.key := "target",
                  Tile("Target")(
                    <.span(
                      // obsSummaryOpt.whenDefined(obs =>
                      //   TargetEditor(obs.target.id).withKey(obs.target.id.toString)
                      // )
                    )
                  )
                )
              )

              if (window.innerWidth <= Constants.TwoPanelCutoff) {
                <.div(
                  ExploreStyles.TreeRGL,
                  <.div(ExploreStyles.Tree, treeInner(observations))
                    .when(state.panels.leftPanelVisible),
                  <.div(ExploreStyles.SinglePanelTile, rightSide)
                    .when(state.panels.rightPanelVisible)
                )
              } else {
                <.div(
                  ExploreStyles.TreeRGL,
                  Resizable(
                    axis = Axis.X,
                    width = treeWidth,
                    height = Option(s.height).getOrElse(0),
                    minConstraints = (Constants.MinLeftPanelWidth.toInt, 0),
                    maxConstraints = (s.width.toInt / 2, 0),
                    onResize = treeResize,
                    resizeHandles = List(ResizeHandleAxis.East),
                    content = tree(observations)
                  ),
                  <.div(^.width := coreWidth.px,
                        ^.left := treeWidth.px,
                        ExploreStyles.SinglePanelTile
                  )(
                    <.div(ExploreStyles.TreeRGLWrapper, rightSide)
                  )
                )
              }
            }
          )
        }
      }
    }
  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .getDerivedStateFromPropsAndState((p, s: Option[State]) =>
        s match {
          case None    =>
            State(TwoPanelState.initial(p.isObsSelected), layouts)
          case Some(s) =>
            if (s.panels.elementSelected =!= p.isObsSelected)
              State.panelSelected.set(p.isObsSelected)(s)
            else s
        }
      )
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build

}
