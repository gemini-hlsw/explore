// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import crystal.react.reuse.*
import eu.timepit.refined.auto.*
import explore.Icons
import explore.common.ConstraintGroupQueries.*
import explore.common.UserPreferencesQueries.*
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.constraints.ConstraintsPanel
import explore.constraints.ConstraintsSummaryTable
import explore.implicits.*
import explore.model.*
import explore.model.enums.AppTab
import explore.model.reusability.*
import explore.observationtree.ConstraintGroupObsList
import explore.optics.*
import explore.optics.all.*
import explore.syntax.ui.*
import explore.undo.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.callback.CallbackCatsEffect.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Program
import lucuma.core.model.User
import lucuma.refined.*
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*
import org.scalajs.dom.window
import queries.common.ConstraintGroupQueriesGQL.*
import queries.common.ObsQueriesGQL
import queries.common.UserPreferencesQueriesGQL.*
import react.common.ReactFnProps
import react.draggable.Axis
import react.fa.*
import react.resizable.*
import react.resizeDetector.*
import react.resizeDetector.hooks.*
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.Button.ButtonProps
import react.semanticui.sizes.*

import scala.collection.immutable.SortedSet
import scala.concurrent.duration.*

case class ConstraintSetTabContents(
  userId:         Option[User.Id],
  programId:      Program.Id,
  focusedObsSet:  Option[ObsIdSet],
  expandedIds:    View[SortedSet[ObsIdSet]],
  listUndoStacks: View[UndoStacks[IO, ConstraintGroupList]],
  // TODO: Clean up the groupUndoStack somewhere, somehow?
  groupUndoStack: View[Map[ObsIdSet, UndoStacks[IO, ConstraintSet]]],
  hiddenColumns:  View[Set[String]],
  summarySorting: View[List[(String, Boolean)]]
)(using val ctx:  AppContextIO)
    extends ReactFnProps(ConstraintSetTabContents.component)

object ConstraintSetTabContents extends TwoResizablePanels {
  private type Props = ConstraintSetTabContents
  given Reusability[Double] = Reusability.double(2.0)

  def readWidthPreference(props: Props, state: View[TwoPanelState]): Callback = {
    import props.given

    (UserAreaWidths.queryWithDefault[IO](
      props.userId,
      ResizableSection.ConstraintSetsTree,
      Constants.InitialTreeWidth.toInt
    ) >>= (w => state.zoom(TwoPanelState.treeWidth).async.set(w.toDouble))).runAsync
  }

  private def renderFn(
    props:              Props,
    state:              View[TwoPanelState],
    resize:             UseResizeDetectorReturn,
    debouncer:          Reusable[UseSingleEffect[IO]]
  )(
    constraintsWithObs: View[ConstraintSummaryWithObervations]
  )(using ctx:          AppContextIO): VdomNode = {

    val treeWidth = state.get.treeWidth.toInt

    def constraintsTree(constraintWithObs: View[ConstraintSummaryWithObervations]) =
      ConstraintGroupObsList(
        constraintWithObs,
        props.programId,
        props.focusedObsSet,
        state.zoom(TwoPanelState.selected).set(SelectedPanel.summary).reuseAlways,
        props.expandedIds,
        props.listUndoStacks
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

    val backButton: VdomNode =
      makeBackButton(props.programId, AppTab.Constraints, ctx, state.zoom(TwoPanelState.selected))

    val (coreWidth, coreHeight) = coreDimensions(resize, treeWidth)

    val rightSide = props.focusedObsSet
      .flatMap(ids =>
        findConstraintGroup(ids, constraintsWithObs.get.constraintGroups).map(cg => (ids, cg))
      )
      .fold[VdomNode] {
        Tile("constraints".refined,
             "Constraints Summary",
             backButton.some,
             key = "constraintsSummary"
        )(renderInTitle =>
          ConstraintsSummaryTable(
            props.programId,
            constraintsWithObs.get.constraintGroups,
            props.hiddenColumns,
            props.summarySorting,
            props.expandedIds,
            renderInTitle
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

        val csView: View[ConstraintSet] =
          cglView
            .zoom(getCs)(modCs)

        val csUndo: View[UndoStacks[IO, ConstraintSet]] =
          props.groupUndoStack.zoom(atMapWithDefault(idsToEdit, UndoStacks.empty))

        val title = idsToEdit.single match
          case Some(id) => s"Observation $id"
          case None     => s"Editing Constraints for ${idsToEdit.size} Observations"

        Tile("constraints".refined, title, backButton.some)(renderInTitle =>
          ConstraintsPanel(idsToEdit.toList, csView, csUndo, renderInTitle)
        )
      }

    makeOneOrTwoPanels(
      treeWidth,
      coreHeight,
      coreWidth,
      state,
      constraintsTree(constraintsWithObs),
      rightSide,
      RightSideCardinality.Single,
      treeResize(props.userId,
                 state.zoom(TwoPanelState.treeWidth),
                 ResizableSection.ConstraintSetsTree,
                 debouncer
      )
    )
  }

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateView(TwoPanelState.initial(SelectedPanel.Uninitialized))
      .useEffectOnMountBy((props, state) => readWidthPreference(props, state))
      .useEffectWithDepsBy((props, state) =>
        (props.focusedObsSet, state.zoom(TwoPanelState.selected).reuseByValue)
      ) { (_, _) => params =>
        val (focusedObsSet, selected) = params
        (focusedObsSet, selected.get) match {
          case (Some(_), _)                 => selected.set(SelectedPanel.editor)
          case (None, SelectedPanel.Editor) => selected.set(SelectedPanel.summary)
          case _                            => Callback.empty
        }
      }
      .useStreamResourceViewOnMountBy { (props, _) =>
        import props.given

        ConstraintGroupObsQuery
          .query(props.programId)
          .map(ConstraintGroupObsQuery.Data.asConstraintSummWithObs.get)
          .reRunOnResourceSignals(
            ObsQueriesGQL.ProgramObservationsEditSubscription.subscribe[IO](props.programId)
          )
      }
      // Measure its size
      .useResizeDetector()
      .useSingleEffect(debounce = 1.second)
      .render { (props, state, constraintsWithObs, resize, debouncer) =>
        import props.given

        <.div(
          constraintsWithObs.render(renderFn(props, state, resize, debouncer) _)
        ).withRef(resize.ref)
      }
}
