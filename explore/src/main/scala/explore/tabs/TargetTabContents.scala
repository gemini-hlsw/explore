// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import scala.collection.immutable.SortedSet

import cats.syntax.all._
import cats.effect.IO
import crystal.react.implicits._
import explore._
import explore.components.Tile
import explore.components.TileButton
import explore.components.ui.ExploreStyles
import explore.utils._
import explore.model.Focused._
import explore.model._
import explore.model.enum.AppTab
import explore.model.reusability._
import explore.observationtree.TargetObsList
import explore.observationtree.TargetObsQueries._
import explore.targeteditor.TargetEditor
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Target
import monocle.macros.Lenses
import org.scalajs.dom.window
import react.common._
import react.common.implicits._
import react.draggable.Axis
import react.resizable._
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.Button.ButtonProps
import react.semanticui.sizes._
import react.sizeme._

final case class TargetTabContents(
  focused:           View[Option[Focused]],
  expandedTargetIds: View[SortedSet[Target.Id]]
) extends ReactProps[TargetTabContents](TargetTabContents.component) {
  def isTargetSelected: Boolean = focused.get.isDefined
}

object TargetTabContents {
  type Props = TargetTabContents
  val TwoPanelCutoff    = 550.0
  val InitialTreeWidth  = 300.0
  val MinLeftPanelWidth = 270.0

  @Lenses
  final case class State(treeWidth: JsNumber, targetSelected: Boolean) {
    val leftPanelVisible: Boolean  = !targetSelected
    val rightPanelVisible: Boolean = targetSelected
  }

  object State {
    // Keep them as def to take the value window.innerWidth at the current time
    def isTwoPanel: Boolean =
      window.innerWidth > TwoPanelCutoff

    def initialPanelWidth(v: Boolean): Double =
      if (isTwoPanel) InitialTreeWidth
      else if (v) 0
      else window.innerWidth

    def initialState(v: Boolean): State =
      State(initialPanelWidth(v), v)

  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive
  implicit val stateReuse: Reusability[State] = Reusability.derive

  protected val component =
    ScalaComponent
      .builder[Props]
      .getDerivedStateFromPropsAndState((p, s: Option[State]) =>
        s match {
          case None    => State.initialState(p.isTargetSelected)
          case Some(s) =>
            if (s.targetSelected =!= p.isTargetSelected) s.copy(targetSelected = p.isTargetSelected)
            else s
        }
      )
      .renderPS { ($, props, state) =>
        AppCtx.withCtx { ctx =>
          implicit val cs = ctx.cs
          val treeResize  =
            (_: ReactEvent, d: ResizeCallbackData) => $.setStateL(State.treeWidth)(d.size.width)
          val treeWidth   = state.treeWidth.toDouble

          // Tree area
          def tree(targetsWithObs:      View[TargetsWithObs]) =
            <.div(^.width := treeWidth.px, ExploreStyles.Tree)(treeInner(targetsWithObs))

          def treeInner(targetsWithObs: View[TargetsWithObs]) =
            <.div(ExploreStyles.TreeBody)(
              TargetObsList(
                targetsWithObs,
                props.focused,
                props.expandedTargetIds
              )
            )

          val backButton = TileButton(
            Button(
              as = <.a,
              size = Mini,
              clazz = ExploreStyles.TargetBackButton |+| ExploreStyles.BlendedButton,
              onClickE = linkOverride[IO, ButtonProps](props.focused.set(none))
            )(^.href := ctx.pageUrl(AppTab.Targets, none), Icons.ChevronLeft.fitted(true))
          )

          TargetObsLiveQuery { targetsWithObs =>
            val targetIdOpt = props.focused.get.collect {
              case FocusedTarget(targetId) => targetId.some
              case FocusedObs(obsId)       => targetsWithObs.get.obs.getElement(obsId).map(_.target.id)
            }.flatten

            React.Fragment(
              SizeMe() { s =>
                val coreWidth            = s.width.toDouble - treeWidth
                val coreHeight: JsNumber = Option(s.height).getOrElse(0)

                val rightSide =
                  Tile(s"Target", movable = false, backButton.some)(
                    <.span(
                      targetIdOpt.whenDefined(targetId =>
                        TargetEditor(targetId).withKey(targetId.toString)
                      )
                    )
                  )

                // It would be nice to make a single component here but it gets hard when you
                // have the resizable element. Instead we have either two panels with a resizable
                // or only one panel at a time (Mobile)
                if (window.innerWidth <= TwoPanelCutoff) {
                  <.div(
                    ExploreStyles.TreeRGL,
                    <.div(ExploreStyles.Tree, treeInner(targetsWithObs))
                      .when(state.leftPanelVisible),
                    <.div(ExploreStyles.SinglePanelTile, rightSide).when(state.rightPanelVisible)
                  )
                } else {
                  <.div(
                    ExploreStyles.TreeRGL,
                    Resizable(
                      axis = Axis.X,
                      width = treeWidth,
                      height = coreHeight,
                      minConstraints = (MinLeftPanelWidth.toInt, 0),
                      maxConstraints = (s.width.toInt / 2, 0),
                      onResize = treeResize,
                      resizeHandles = List(ResizeHandleAxis.East),
                      content = tree(targetsWithObs),
                      clazz = ExploreStyles.ResizableSeparator
                    ),
                    <.div(
                      ExploreStyles.SinglePanelTile,
                      ^.width := coreWidth.px,
                      ^.left := treeWidth.px,
                      rightSide
                    )
                  )
                }
              }
            )
          }
        }
      }
      .configure(Reusability.shouldComponentUpdate)
      .build

}
