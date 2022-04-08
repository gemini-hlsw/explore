// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all._
import crystal.react.ReuseView
import crystal.react.View
import crystal.react.hooks._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.Icons
import explore.common.ConstraintGroupQueries._
import explore.common.UserPreferencesQueries._
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
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.ui.reusability._
import lucuma.ui.utils._
import org.scalajs.dom.window
import queries.common.UserPreferencesQueriesGQL._
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
  programId:        Program.Id,
  focusedObsSet:    Option[ObsIdSet],
  expandedIds:      ReuseView[SortedSet[ObsIdSet]],
  listUndoStacks:   ReuseView[UndoStacks[IO, ConstraintGroupList]],
  // TODO: Clean up the groupUndoStack somewhere, somehow?
  groupUndoStack:   ReuseView[Map[ObsIdSet, UndoStacks[IO, ConstraintSet]]],
  hiddenColumns:    ReuseView[Set[String]],
  summarySorting:   ReuseView[List[(String, Boolean)]],
  size:             ResizeDetector.Dimensions
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[ConstraintSetTabContents](ConstraintSetTabContents.component)

object ConstraintSetTabContents {
  type Props = ConstraintSetTabContents
  type State = TwoPanelState[ObsIdSet]

  implicit val propsReuse: Reusability[Props] = Reusability.derive
  implicit val stateReuse: Reusability[State] = Reusability.derive

  val treeWidthLens = TwoPanelState.treeWidth[ObsIdSet]
  val selectedLens  = TwoPanelState.selected[ObsIdSet]

  def readWidthPreference(props: Props, state: View[State]): Callback = {
    implicit val ctx = props.ctx

    (UserAreaWidths.queryWithDefault[IO](
      props.userId,
      ResizableSection.ConstraintSetsTree,
      Constants.InitialTreeWidth.toInt
    ) >>= (w => state.zoom(treeWidthLens).async.set(w.toDouble))).runAsync
  }

  protected def renderFn(
    props:              Props,
    state:              ReuseView[State],
    constraintsWithObs: ReuseView[ConstraintSummaryWithObervations]
  )(implicit ctx:       AppContextIO): VdomNode = {
    val treeResize =
      (_: ReactEvent, d: ResizeCallbackData) =>
        (state.zoom(treeWidthLens).set(d.size.width.toDouble).to[IO] *>
          UserWidthsCreation
            .storeWidthPreference[IO](props.userId,
                                      ResizableSection.ConstraintSetsTree,
                                      d.size.width
            )).runAsync
          .debounce(1.second)

    val treeWidth = state.get.treeWidth.toInt

    def tree(constraintWithObs: ReuseView[ConstraintSummaryWithObervations]) =
      <.div(^.width := treeWidth.px, ExploreStyles.Tree |+| ExploreStyles.ResizableSinglePanel)(
        treeInner(constraintWithObs)
      )

    def treeInner(constraintWithObs: ReuseView[ConstraintSummaryWithObervations]) =
      <.div(ExploreStyles.TreeBody)(
        ConstraintGroupObsList(
          constraintWithObs,
          props.programId,
          props.focusedObsSet,
          state.zoom(selectedLens).set(SelectedPanel.summary).reuseAlways,
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

      updateExpanded
    }

    val backButton = Reuse.always[VdomNode](
      Button(
        as = <.a,
        size = Mini,
        compact = true,
        basic = true,
        clazz = ExploreStyles.TileBackButton |+| ExploreStyles.BlendedButton,
        onClickE = linkOverride[ButtonProps](
          ctx.pushPage(AppTab.Constraints, props.programId, none, none) >>
            state.zoom(selectedLens).set(SelectedPanel.tree)
        )
      )(^.href := ctx.pageUrl(AppTab.Constraints, props.programId, none, none), Icons.ChevronLeft)
    )

    val coreWidth  = props.size.width.getOrElse(0) - treeWidth
    val coreHeight = props.size.height.getOrElse(0)

    val rightSide = state.get.selected.optValue
      .flatMap(ids =>
        findConstraintGroup(ids, constraintsWithObs.get.constraintGroups).map(cg => (ids, cg))
      )
      .fold[VdomNode] {
        Tile("constraints", "Constraints Summary", backButton.some, key = "constraintsSummary")(
          Reuse.by((props.programId, constraintsWithObs, props.hiddenColumns))(
            (renderInTitle: Tile.RenderInTitle) =>
              ConstraintsSummaryTable(
                props.programId,
                constraintsWithObs.get.constraintGroups,
                props.hiddenColumns,
                props.summarySorting,
                props.expandedIds,
                renderInTitle
              )
          )
        )
      } { case (idsToEdit, constraintGroup) =>
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

        val csView: ReuseView[ConstraintSet] =
          cglView
            .zoom(getCs)(modCs)

        val csUndo: ReuseView[UndoStacks[IO, ConstraintSet]] =
          props.groupUndoStack.zoom(atMapWithDefault(idsToEdit, UndoStacks.empty))

        val title = idsToEdit.single match {
          case Some(id) => s"Observation $id"
          case None     => s"Editing Constraints for ${idsToEdit.size} Observations"
        }

        Tile("constraints", title, backButton.some)(
          (csView, csUndo).curryReusing.in((csView_, csUndo_, renderInTitle) =>
            <.div(ConstraintsPanel(idsToEdit.toList, csView_, csUndo_, renderInTitle))
          )
        )
      }

    if (window.canFitTwoPanels) {
      <.div(^.key := "constraints-base")(
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
          width = treeWidth.toDouble,
          height = coreHeight.toDouble,
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

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateViewWithReuse(TwoPanelState.initial[ObsIdSet](SelectedPanel.Uninitialized))
      .useEffectOnMountBy((props, state) => readWidthPreference(props, state))
      .useEffectWithDepsBy((props, state) =>
        (props.focusedObsSet, state.zoom(selectedLens).value.reuseByValue)
      ) { (_, _) => params =>
        val (focusedObsSet, selected) = params
        (focusedObsSet, selected.get) match {
          case (Some(obsIdSet), _)             => selected.set(SelectedPanel.editor(obsIdSet))
          case (None, SelectedPanel.Editor(_)) => selected.set(SelectedPanel.Summary)
          case _                               => Callback.empty
        }
      }
      .renderWithReuse { (props, state) =>
        implicit val ctx = props.ctx

        ConstraintGroupLiveQuery(props.programId)(
          Reuse(renderFn _)(props, state)
        )
      }
}
