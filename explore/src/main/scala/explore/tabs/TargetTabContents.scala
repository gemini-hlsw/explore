// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.syntax.all._
import crystal.react.implicits._
import explore._
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.model.Focused._
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
import react.resizable._
import react.sizeme._

object TargetTabContents {
  type Props = View[Option[Focused]]

  final case class State(treeWidth: JsNumber)

  implicit val stateReuse: Reusability[State] = Reusability.derive

  class Backend($ : BackendScope[Props, State]) {
    def render(props: Props, state: State) = {
      val treeResize = (_: ReactEvent, d: ResizeCallbackData) => $.setState(State(d.size.width))
      val treeWidth  = state.treeWidth.toDouble

      // Tree area
      def tree(targetsWithObs: View[TargetsWithObs]) =
        <.div(^.width := treeWidth.px, ExploreStyles.Tree)(
          <.div(ExploreStyles.TreeBodyOuter)(
            <.div(ExploreStyles.TreeBodyInner)(
              TargetObsList(
                targetsWithObs,
                props
              )
            )
          )
        )

      TargetObsLiveQuery { targetsWithObs =>
        val targetIdOpt = props.get.collect {
          case FocusedTarget(targetId) => targetId.some
          case FocusedObs(obsId)       => targetsWithObs.get.obs.getElement(obsId).map(_.target.id)
        }.flatten

        <.div(
          ExploreStyles.SinglePanelArea,
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
                content = tree(targetsWithObs)
              ),
              <.div(
                ExploreStyles.SinglePanelTile,
                ^.width := coreWidth.px,
                ^.left := treeWidth.px,
                Tile("Target Position", movable = false)(
                  <.span(
                    targetIdOpt.whenDefined(targetId =>
                      TargetEditor(targetId).withKey(targetId.toString)
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
