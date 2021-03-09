// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import explore.AppCtx
import explore.Icons
import explore.common.UserPreferencesQueries._
import explore.components.Tile
import explore.components.TileButton
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.Focused._
import explore.model._
import explore.model.enum.AppTab
import explore.model.reusability._
import explore.observationtree.TargetObsList
import explore.observationtree.TargetObsQueries._
import explore.targeteditor.TargetEditor
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.builder.Lifecycle.ComponentDidMount
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.User
import lucuma.ui.reusability._
import lucuma.ui.utils._
import org.scalajs.dom.window
import react.common._
import react.common.implicits._
import react.draggable.Axis
import react.resizable._
import react.resizeDetector.ResizeDetector
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.Button.ButtonProps
import react.semanticui.sizes._

import scala.concurrent.duration._

final case class TargetTabContents(
  userId:                ViewOpt[User.Id],
  focused:               View[Option[Focused]],
  targetViewExpandedIds: View[TargetViewExpandedIds],
  size:                  ResizeDetector.Dimensions
) extends ReactProps[TargetTabContents](TargetTabContents.component) {
  def isTargetSelected: Boolean = focused.get.isDefined
}

object TargetTabContents {
  type Props = TargetTabContents
  type State = TwoPanelState

  implicit val propsReuse: Reusability[Props] = Reusability.derive

  def readWidthPreference($ : ComponentDidMount[Props, State, Unit]): Callback =
    AppCtx.flatMap { implicit ctx =>
      UserAreaWidths.queryWithDefault[IO]($.props.userId.get,
                                          ResizableSection.TargetsTree,
                                          Constants.InitialTreeWidth.toInt
      ) >>= $.setStateLIn[IO](TwoPanelState.treeWidth)
    }.runAsyncCB

  protected val component =
    ScalaComponent
      .builder[Props]
      .getDerivedStateFromPropsAndState((p, s: Option[State]) =>
        s match {
          case None    => TwoPanelState.initial(p.isTargetSelected)
          case Some(s) =>
            if (s.elementSelected =!= p.isTargetSelected)
              s.copy(elementSelected = p.isTargetSelected)
            else s
        }
      )
      .renderPS { ($, props, state) =>
        AppCtx.withCtx { implicit ctx =>
          val treeResize =
            (_: ReactEvent, d: ResizeCallbackData) =>
              ($.setStateLIn[IO](TwoPanelState.treeWidth)(d.size.width) *>
                UserWidthsCreation
                  .storeWidthPreference[IO](props.userId.get,
                                            ResizableSection.TargetsTree,
                                            d.size.width
                  )).runAsyncCB
                .debounce(1.second)

          val treeWidth = state.treeWidth.toInt

          // Tree area
          def tree(objectsWithObs: View[TargetsAndAsterismsWithObs]) =
            <.div(^.width := treeWidth.px,
                  ExploreStyles.Tree |+| ExploreStyles.ResizableSinglePanel
            )(treeInner(objectsWithObs))

          def treeInner(objectsWithObs: View[TargetsAndAsterismsWithObs]) =
            <.div(ExploreStyles.TreeBody)(
              TargetObsList(
                objectsWithObs,
                props.focused,
                props.targetViewExpandedIds
              )
            )

          val backButton = TileButton(
            Button(
              as = <.a,
              size = Mini,
              compact = true,
              basic = true,
              clazz = ExploreStyles.TileBackButton |+| ExploreStyles.BlendedButton,
              onClickE = linkOverride[IO, ButtonProps](props.focused.set(none))
            )(^.href := ctx.pageUrl(AppTab.Targets, none), Icons.ChevronLeft.fitted(true))
          )

          TargetObsLiveQuery { objectsWithObs =>
            val targetIdOpt = props.focused.get.collect {
              case FocusedTarget(targetId) => targetId.some
              case FocusedObs(obsId)       =>
                objectsWithObs.get.observations
                  .getElement(obsId)
                  .flatMap(_.pointingId.flatMap(_.toOption))
            }.flatten

            val coreWidth  = props.size.width.getOrElse(0) - treeWidth
            val coreHeight = props.size.height.getOrElse(0)

            val rightSide =
              Tile(s"Target", backButton.some)(
                (props.userId.get, targetIdOpt).mapN { case (uid, tid) =>
                  TargetEditor(uid, tid).withKey(tid.show)
                }
              )

            // It would be nice to make a single component here but it gets hard when you
            // have the resizable element. Instead we have either two panels with a resizable
            // or only one panel at a time (Mobile)
            if (window.innerWidth <= Constants.TwoPanelCutoff) {
              <.div(
                ExploreStyles.TreeRGL,
                <.div(ExploreStyles.Tree, treeInner(objectsWithObs))
                  .when(state.leftPanelVisible),
                <.div(^.key := "target-right-side", ExploreStyles.SinglePanelTile)(
                  rightSide
                ).when(state.rightPanelVisible)
              )
            } else {
              <.div(
                ExploreStyles.TreeRGL,
                Resizable(
                  axis = Axis.X,
                  width = treeWidth,
                  height = coreHeight,
                  minConstraints = (Constants.MinLeftPanelWidth.toInt, 0),
                  maxConstraints = (props.size.width.getOrElse(0) / 2, 0),
                  onResize = treeResize,
                  resizeHandles = List(ResizeHandleAxis.East),
                  content = tree(objectsWithObs),
                  clazz = ExploreStyles.ResizableSeparator
                ),
                <.div(^.key := "target-right-side",
                      ExploreStyles.SinglePanelTile,
                      ^.width := coreWidth.px,
                      ^.left := treeWidth.px
                )(
                  rightSide
                )
              )
            }
          }
        }
      }
      .componentDidMount(readWidthPreference)
      .configure(Reusability.shouldComponentUpdate)
      .build

}
