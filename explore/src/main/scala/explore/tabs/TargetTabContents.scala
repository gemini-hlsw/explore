// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import explore.GraphQLSchemas.UserPreferencesDB
import explore.GraphQLSchemas.UserPreferencesDB.Types._
import explore.components.ui.ExploreStyles
import explore.components.{ Tile, TileButton }
import explore.AppCtx
import explore.Icons
import explore.implicits._
import explore.model.Focused._
import explore.model._
import explore.model.enum.AppTab
import explore.model.reusability._
import explore.observationtree.TargetObsList
import explore.observationtree.TargetObsQueries._
import explore.target.UserPreferencesQueries._
import explore.targeteditor.TargetEditor
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.raw.JsNumber
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.{ Target, User }
import lucuma.ui.reusability._
import lucuma.ui.utils._
import org.scalajs.dom.window
import react.common._
import react.common.implicits._
import react.draggable.Axis
import react.resizable._
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.Button.ButtonProps
import react.semanticui.sizes._
import react.sizeme._

import scala.collection.immutable.SortedSet
import scala.concurrent.duration._
import japgolly.scalajs.react.component.builder.Lifecycle.ComponentDidMount
import clue.GraphQLClient
import cats.effect.Effect

final case class TargetTabContents(
  userId:            ViewOpt[User.Id],
  focused:           View[Option[Focused]],
  expandedTargetIds: View[SortedSet[Target.Id]]
) extends ReactProps[TargetTabContents](TargetTabContents.component) {
  def isTargetSelected: Boolean = focused.get.isDefined
}

object TargetTabContents {
  type Props = TargetTabContents
  type State = TwoPanelState

  implicit val propsReuse: Reusability[Props] = Reusability.derive

  def readWidthPreference($ : ComponentDidMount[Props, State, Unit]): Callback =
    AppCtx.withCtx { implicit ctx =>
      UserAreaWidths
        .queryWithDefault[IO]($.props.userId.get,
                              ResizableSection.TargetsTree,
                              Constants.InitialTreeWidth.toInt
        )
        .runAsyncAndThenCB(w => $.setStateL(TwoPanelState.treeWidth)(w))
    }

  def storeWidthPreference[F[_]: Effect](props: Props, width: Int)(implicit
    cl:                                         GraphQLClient[F, UserPreferencesDB]
  ): Callback =
    props.userId.get.map { i =>
      UserWidthsCreation
        .execute[F](
          WidthUpsertInput(i, ResizableSection.TargetsTree, width)
        )
        .runAsyncAndForgetCB
    }.getOrEmpty

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
              $.setStateL(TwoPanelState.treeWidth)(d.size.width) *>
                storeWidthPreference[IO](props, d.size.width).debounce(1.second)

          val treeWidth = state.treeWidth.toDouble

          def tree(targetsWithObs: View[TargetsWithObs]) =
            <.div(^.width := treeWidth.px,
                  ExploreStyles.Tree |+| ExploreStyles.ResizableSinglePanel
            )(treeInner(targetsWithObs))

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
              compact = true,
              basic = true,
              clazz = ExploreStyles.TileBackButton |+| ExploreStyles.BlendedButton,
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
                  Tile(s"Target", backButton.some)(
                    <.span(
                      targetIdOpt.whenDefined(targetId =>
                        TargetEditor(targetId).withKey(targetId.toString)
                      )
                    )
                  )

                // It would be nice to make a single component here but it gets hard when you
                // have the resizable element. Instead we have either two panels with a resizable
                // or only one panel at a time (Mobile)
                if (window.innerWidth <= Constants.TwoPanelCutoff) {
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
                      minConstraints = (Constants.MinLeftPanelWidth.toInt, 0),
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
      .componentDidMount(readWidthPreference)
      .configure(Reusability.shouldComponentUpdate)
      .build

}
