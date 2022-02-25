// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all._
import crystal.ViewF
import crystal.react.View
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.Icons
import explore.common.ConstraintGroupQueries._
import explore.common.UserPreferencesQueries._
import explore.common.UserPreferencesQueriesGQL._
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.constraints.ConstraintsPanel
import explore.constraints.ConstraintsSummaryTable
import explore.implicits._
import explore.model._
import explore.model.enum.AppTab
import explore.model.reusability._
import explore.observationtree.ConstraintGroupObsList
import explore.optics._
import explore.syntax.ui._
import explore.undo._
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
  focusedObs:       View[Option[Observation.Id]],
  expandedIds:      View[SortedSet[ObsIdSet]],
  listUndoStacks:   View[UndoStacks[IO, ConstraintGroupList]],
  // TODO: Clean up the groupUndoStack somewhere, somehow?
  groupUndoStack:   View[Map[ObsIdSet, UndoStacks[IO, ConstraintSet]]],
  hiddenColumns:    View[Set[String]],
  summarySorting:   View[List[(String, Boolean)]],
  size:             ResizeDetector.Dimensions
)(implicit val ctx: AppContextIO)
    extends ReactProps[ConstraintSetTabContents](ConstraintSetTabContents.component)

object ConstraintSetTabContents {
  type Props = ConstraintSetTabContents
  type State = TwoPanelState[ObsIdSet]

  implicit val propsReuse: Reusability[Props] = Reusability.derive
  implicit val stateReuse: Reusability[State] = Reusability.derive

  val treeWidthLens = TwoPanelState.treeWidth[ObsIdSet]
  val selectedLens  = TwoPanelState.selected[ObsIdSet]

  def readWidthPreference(
    $ : ComponentDidMount[Props, State, _]
  ): Callback = {
    implicit val ctx = $.props.ctx
    (UserAreaWidths.queryWithDefault[IO]($.props.userId,
                                         ResizableSection.ConstraintSetsTree,
                                         Constants.InitialTreeWidth.toInt
    ) >>= $.setStateLIn[IO](treeWidthLens)).runAsync
  }

  protected def renderFn(
    props:              Props,
    state:              View[State],
    constraintsWithObs: View[ConstraintSummaryWithObervations]
  )(implicit ctx:       AppContextIO): VdomNode = {
    val treeResize =
      (_: ReactEvent, d: ResizeCallbackData) =>
        (state.zoom(treeWidthLens).set(d.size.width).to[IO] *>
          UserWidthsCreation
            .storeWidthPreference[IO](props.userId,
                                      ResizableSection.ConstraintSetsTree,
                                      d.size.width
            )).runAsync
          .debounce(1.second)

    val treeWidth = state.get.treeWidth.toInt

    def tree(constraintWithObs: View[ConstraintSummaryWithObervations]) =
      <.div(^.width := treeWidth.px, ExploreStyles.Tree |+| ExploreStyles.ResizableSinglePanel)(
        treeInner(constraintWithObs)
      )

    def treeInner(constraintWithObs: View[ConstraintSummaryWithObervations]) =
      <.div(ExploreStyles.TreeBody)(
        ConstraintGroupObsList(constraintWithObs,
                               props.focusedObs,
                               state.zoom(selectedLens),
                               props.expandedIds,
                               props.listUndoStacks
        )
      )

    def findConstraintGroup(
      obsIds: ObsIdSet,
      cgl:    ConstraintGroupList
    ): Option[ConstraintGroup] =
      cgl.find(_._1.intersect(obsIds).nonEmpty).map(_._2)

    def onModSummaryWithObs(
      groupObsIds:  ObsIdSet,
      editedObsIds: ObsIdSet
    )(cswo:         ConstraintSummaryWithObervations): Callback = {
      val groupList = cswo.constraintGroups

      // If we're editing at the group level (even a group of 1) and it no longer exists
      // (probably due to a merger), just go to the summary.
      val updateSelection = props.focusedObs.get match {
        case Some(_) => Callback.empty
        case None    =>
          groupList
            .get(editedObsIds)
            .fold(state.zoom(selectedLens).set(SelectedPanel.summary))(_ => Callback.empty)
      }

      val updateExpanded = findConstraintGroup(editedObsIds, groupList).fold(Callback.empty) { cg =>
        // We should always find the constraint group.
        // If a group was edited while closed and it didn't create a merger, keep it closed,
        // otherwise expand all affected groups.
        props.expandedIds
          .mod { eids =>
            val withOld       =
              if (groupObsIds === editedObsIds) eids
              else eids + groupObsIds.removeUnsafe(editedObsIds)
            val withOldAndNew =
              if (editedObsIds === cg.obsIds && editedObsIds === groupObsIds) withOld
              else withOld + cg.obsIds

            withOldAndNew.filter(ids => groupList.contains(ids)) // clean up
          }
      }

      updateSelection >> updateExpanded
    }

    val backButton = Reuse.always[VdomNode](
      Button(
        as = <.a,
        size = Mini,
        compact = true,
        basic = true,
        clazz = ExploreStyles.TileBackButton |+| ExploreStyles.BlendedButton,
        onClickE = linkOverride[ButtonProps](state.zoom(selectedLens).set(SelectedPanel.tree))
      )(^.href := ctx.pageUrl(AppTab.Constraints, none, none), Icons.ChevronLeft)
    )

    val coreWidth  = props.size.width.getOrElse(0) - treeWidth
    val coreHeight = props.size.height.getOrElse(0)

    val rightSide = state.get.selected.optValue
      .flatMap(ids =>
        findConstraintGroup(ids, constraintsWithObs.get.constraintGroups).map(cg => (ids, cg))
      )
      .fold[VdomNode](
        Tile("constraints", "Constraints Summary", backButton.some)(
          Reuse.by((constraintsWithObs, props.hiddenColumns))((renderInTitle: Tile.RenderInTitle) =>
            ConstraintsSummaryTable(
              constraintsWithObs.get.constraintGroups,
              props.hiddenColumns,
              props.summarySorting,
              state.zoom(TwoPanelState.selected),
              props.focusedObs,
              props.expandedIds,
              renderInTitle
            )
          )
        )
      ) { case (idsToEdit, constraintGroup) =>
        val groupObsIds   = constraintGroup.obsIds
        val constraintSet = constraintGroup.constraintSet
        val cglView       = constraintsWithObs
          .withOnMod(onModSummaryWithObs(groupObsIds, idsToEdit))
          .zoom(ConstraintSummaryWithObervations.constraintGroups)

        val getCs: ConstraintGroupList => ConstraintSet = _ => constraintSet

        def modCs(mod: ConstraintSet => ConstraintSet): ConstraintGroupList => ConstraintGroupList =
          cgl =>
            findConstraintGroup(idsToEdit, cgl)
              .map { cg =>
                val newCg        = ConstraintGroup.constraintSet.modify(mod)(cg)
                // see if the edit caused a merger
                val mergeWithIds = cgl
                  .find { case (ids, group) =>
                    !ids.intersects(idsToEdit) && group.constraintSet === newCg.constraintSet
                  }
                  .map(_._1)

                // If we're editing an observation within a larger group, we need a split
                val splitList =
                  if (idsToEdit === groupObsIds)
                    cgl.updated(groupObsIds, newCg) // otherwise, just update current group
                  else {
                    val diffIds = groupObsIds.removeUnsafe(idsToEdit)
                    cgl
                      .removed(groupObsIds)
                      .updated(idsToEdit, ConstraintGroup(newCg.constraintSet, idsToEdit))
                      .updated(diffIds, ConstraintGroup(cg.constraintSet, diffIds))
                  }

                mergeWithIds.fold(splitList) { idsToMerge =>
                  val combined = idsToMerge ++ idsToEdit
                  splitList
                    .removed(idsToMerge)
                    .removed(idsToEdit)
                    .updated(combined, ConstraintGroup(newCg.constraintSet, combined))
                }
              }
              .getOrElse(cgl) // shouldn't happen

        val csView: View[ConstraintSet] =
          cglView
            .zoom(getCs)(modCs)

        val csUndo: View[UndoStacks[IO, ConstraintSet]] =
          props.groupUndoStack.zoom(atMapWithDefault(idsToEdit, UndoStacks.empty))

        val title = props.focusedObs.get match {
          case Some(id) => s"Observation $id"
          case None     =>
            val titleSfx = if (idsToEdit.size == 1) "" else "s"
            s"Editing Constraints for ${idsToEdit.size} Observation$titleSfx"
        }

        Tile("constraints", title, backButton.some)(
          (csView, csUndo).curryReusing.in((csView_, csUndo_, renderInTitle) =>
            <.div(ConstraintsPanel(idsToEdit.toList, csView_, csUndo_, renderInTitle))
          )
        )
      }

    if (window.canFitTwoPanels) {
      <.div(
        ExploreStyles.TreeRGL,
        <.div(ExploreStyles.Tree, tree(constraintsWithObs))
          .when(state.get.selected.leftPanelVisible),
        <.div(^.key := "constraintset-right-side", ExploreStyles.SinglePanelTile)(
          rightSide
        ).when(state.get.selected.rightPanelVisible)
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
        <.div(^.key   := "constraintset-right-side",
              ExploreStyles.SinglePanelTile,
              ^.width := coreWidth.px,
              ^.left  := treeWidth.px
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
        Reuse(renderFn _)(props, ViewF.fromState($))
      )
    }
  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .initialState(TwoPanelState.initial[ObsIdSet](SelectedPanel.Uninitialized))
      .renderBackend[Backend]
      .componentDidMount(readWidthPreference)
      .configure(Reusability.shouldComponentUpdate)
      .build
}
