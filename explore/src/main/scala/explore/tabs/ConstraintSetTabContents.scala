// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all._
import crystal.ViewF
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.ConstraintGroupQueries._
import explore.common.UserPreferencesQueries._
import explore.common.UserPreferencesQueriesGQL._
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.constraints.ConstraintsPanel
import explore.implicits._
import explore.model._
import explore.model.Focused._
import explore.model.enum.AppTab
import explore.model.reusability._
import explore.optics._
import explore.undo._
import explore.observationtree.ConstraintGroupObsList
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.builder.Lifecycle.ComponentDidMount
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
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

import scala.collection.immutable.SortedSet
import scala.concurrent.duration._

final case class ConstraintSetTabContents(
  userId:           Option[User.Id],
  focused:          View[Option[Focused]],
  expandedIds:      View[SortedSet[SortedSet[Observation.Id]]],
  listUndoStacks:   View[UndoStacks[IO, ConstraintGroupList]],
  // TODO: Clean up the bulkUndoStack somewhere, somehow?
  bulkUndoStack:    View[Map[SortedSet[Observation.Id], UndoStacks[IO, ConstraintSet]]],
  size:             ResizeDetector.Dimensions
)(implicit val ctx: AppContextIO)
    extends ReactProps[ConstraintSetTabContents](ConstraintSetTabContents.component) {
  def isSelected: Boolean = focused.get.collect { case Focused.FocusedObs(_) =>
    ()
  }.isDefined
}

object ConstraintSetTabContents {
  type Props = ConstraintSetTabContents
  type State = TwoPanelState

  implicit val propsReuse: Reusability[Props] = Reusability.derive

  def readWidthPreference(
    $ : ComponentDidMount[Props, State, _]
  ): Callback = {
    implicit val ctx = $.props.ctx
    (UserAreaWidths.queryWithDefault[IO]($.props.userId,
                                         ResizableSection.ConstraintSetsTree,
                                         Constants.InitialTreeWidth.toInt
    ) >>= $.setStateLIn[IO](TwoPanelState.treeWidth)).runAsyncCB
  }

  protected def renderFn(
    props:              Props,
    state:              View[State],
    innerWidth:         Double,
    constraintsWithObs: View[ConstraintSummaryWithObervations]
  )(implicit ctx:       AppContextIO): VdomNode = {
    val treeResize =
      (_: ReactEvent, d: ResizeCallbackData) =>
        (state.zoom(TwoPanelState.treeWidth).set(d.size.width).to[IO] *>
          UserWidthsCreation
            .storeWidthPreference[IO](props.userId,
                                      ResizableSection.ConstraintSetsTree,
                                      d.size.width
            )).runAsyncCB
          .debounce(1.second)

    val treeWidth = state.get.treeWidth.toInt

    def tree(constraintWithObs: View[ConstraintSummaryWithObervations]) =
      <.div(^.width := treeWidth.px, ExploreStyles.Tree |+| ExploreStyles.ResizableSinglePanel)(
        treeInner(constraintWithObs)
      )

    def treeInner(constraintWithObs: View[ConstraintSummaryWithObervations]) =
      <.div(ExploreStyles.TreeBody)(
        ConstraintGroupObsList(constraintWithObs,
                               props.focused,
                               props.expandedIds,
                               props.listUndoStacks
        )
      )

    val backButton = Reuse.always[VdomNode](
      Button(
        as = <.a,
        size = Mini,
        compact = true,
        basic = true,
        clazz = ExploreStyles.TileBackButton |+| ExploreStyles.BlendedButton,
        onClickE = linkOverride[ButtonProps](props.focused.set(none))
      )(^.href := ctx.pageUrl(AppTab.Constraints, none), Icons.ChevronLeft)
    )

    val coreWidth  = props.size.width.getOrElse(0) - treeWidth
    val coreHeight = props.size.height.getOrElse(0)

    val rightSide = props.focused.get
      .flatMap {
        case FocusedObs(obsId) => obsId.some
        case _                 => none
      }
      .flatMap(id => constraintsWithObs.get.constraintGroups.find { case (k, _) => k.contains(id) })
      .fold[VdomNode](
        <.div(ExploreStyles.HVCenter |+| ExploreStyles.EmptyTreeContent,
              <.div("Select a constraint or observation")
        )
      ) { case (obsIds, constraintGroup) =>
        val constraintSet                               = constraintGroup.constraintSet
        val cglView                                     = constraintsWithObs.zoom(ConstraintSummaryWithObervations.constraintGroups)
        val getCs: ConstraintGroupList => ConstraintSet = _ => constraintSet
        def modCs(mod: ConstraintSet => ConstraintSet): ConstraintGroupList => ConstraintGroupList =
          cgl =>
            cgl
              .get(obsIds)
              .fold(cgl)(cg => cgl.updated(obsIds, ConstraintGroup.constraintSet.modify(mod)(cg)))
        val csView: View[ConstraintSet]                 = cglView.zoom(getCs)(modCs)
        val csUndo: View[UndoStacks[IO, ConstraintSet]] =
          props.bulkUndoStack.zoom(atMapWithDefault(obsIds, UndoStacks.empty))

        Tile(NonEmptyString.unsafeFrom(obsIds.mkString("_")), "Constraints", backButton.some)(
          (csView, csUndo).curryReusing.in((csView_, csUndo_, renderInTitle) =>
            <.div(ConstraintsPanel(obsIds.toList, csView_, csUndo_, renderInTitle))
          )
        )
      }

    if (innerWidth <= Constants.TwoPanelCutoff) {
      <.div(
        ExploreStyles.TreeRGL,
        <.div(ExploreStyles.Tree, tree(constraintsWithObs))
          .when(state.get.leftPanelVisible),
        <.div(^.key := "constraintset-right-side", ExploreStyles.SinglePanelTile)(
          rightSide
        ).when(state.get.rightPanelVisible)
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
          content = tree(constraintsWithObs),
          clazz = ExploreStyles.ResizableSeparator
        ),
        <.div(^.key := "constraintset-right-side",
              ExploreStyles.SinglePanelTile,
              ^.width := coreWidth.px,
              ^.left := treeWidth.px
        )(
          rightSide
        )
      )
    }
  }

  protected implicit val innerWidthReuse = Reusability.double(2.0)

  protected class Backend($ : BackendScope[Props, State]) {
    def render(props: Props) = {
      implicit val ctx = props.ctx
      ConstraintGroupLiveQuery(
        Reuse(renderFn _)(props, ViewF.fromStateSyncIO($), window.innerWidth)
      )
    }
  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .getDerivedStateFromPropsAndState((p, s: Option[State]) =>
        s match {
          case None    => TwoPanelState.initial(p.isSelected)
          case Some(s) =>
            if (s.elementSelected =!= p.isSelected) s.copy(elementSelected = p.isSelected)
            else s
        }
      )
      .renderBackend[Backend]
      .componentDidMount(readWidthPreference)
      .configure(Reusability.shouldComponentUpdate)
      .build
}
