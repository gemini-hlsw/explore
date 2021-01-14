// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import explore._
import explore.components.ui.ExploreStyles
import explore.components.{ Tile, TileButton }
import explore.model.Focused.FocusedObs
import explore.model._
import explore.model.enum.AppTab
import explore.model.reusability._
import explore.observationtree.ObsList
import explore.observationtree.ObsQueries._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.ui.reusability._
import lucuma.ui.utils._
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

final case class ObsTabContents(
  focused: View[Option[Focused]]
) extends ReactProps[ObsTabContents](ObsTabContents.component) {
  def isObsSelected: Boolean = focused.get.isDefined
}

object ObsTabContents {
  private val layoutLg: Layout = Layout(
    List(
      LayoutItem(x = 0, y = 0, w = 12, h = 16, i = "overview"),
      LayoutItem(x = 0, y = 8, w = 12, h = 8, i = "target")
    )
  )

  private val layoutMd: Layout = Layout(
    List(
      LayoutItem(x = 0, y = 0, w = 12, h = 8, i = "overview"),
      LayoutItem(x = 0, y = 8, w = 12, h = 8, i = "target")
    )
  )

  private val layouts: Map[BreakpointName, (JsNumber, JsNumber, Layout)] =
    Map(
      (BreakpointName.lg, (1200, 12, layoutLg)),
      (BreakpointName.md, (996, 10, layoutMd))
      // (BreakpointName.sm, (768, 8, layout)),
      // (BreakpointName.xs, (480, 6, layout))
    )

  type Props = ObsTabContents
  type State = TwoPanelState

  implicit val propsReuse: Reusability[Props] = Reusability.derive

  class Backend($ : BackendScope[Props, State]) {
    def render(props: Props, state: State) = {
      AppCtx.withCtx { ctx =>
        val treeResize = (_: ReactEvent, d: ResizeCallbackData) =>
          $.setStateL(TwoPanelState.treeWidth)(d.size.width)
        val treeWidth  = state.treeWidth.toDouble

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
                rowHeight = 30,
                draggableHandle = s".${ExploreStyles.TileTitleMenu.htmlClass}",
                // onLayoutChange = (a, b) => Callback.log(a.toString) *> Callback.log(b.toString),
                layouts = layouts
              )(
                <.div(
                  ^.key := "overview",
                  Tile("Overview", movable = false, backButton.some)(
                    <.span(
                      // obsSummaryOpt.whenDefined(obs =>
                      //   TargetEditor(obs.target.id).withKey(obs.target.id.toString)
                      // )
                    )
                  )
                ),
                <.div(
                  ^.key := "target",
                  Tile("Target", movable = false, None)(
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
                    .when(state.leftPanelVisible),
                  <.div(ExploreStyles.SinglePanelTile, rightSide).when(state.rightPanelVisible)
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
            TwoPanelState.initial(p.isObsSelected)
          case Some(s) =>
            if (s.elementSelected =!= p.isObsSelected)
              s.copy(elementSelected = p.isObsSelected)
            else s
        }
      )
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build

}
