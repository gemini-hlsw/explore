// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all._
import clue.TransactionalClient
import crystal.react.View
import crystal.react.reuse.Reuse
import explore.Icons
import explore.common.ConstraintGroupQueries._
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.implicits._
import explore.model.ConstraintGroup
import explore.model.Focused
import explore.model.ObsIdSet
import explore.model.display._
import explore.model.enums.AppTab
import explore.undo.UndoContext
import explore.undo._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.syntax.all._
import lucuma.schemas.ObservationDB
import monocle.Focus
import monocle.Lens
import mouse.boolean._
import react.beautifuldnd._
import react.common._
import react.common.implicits._
import react.fa.FontAwesomeIcon
import react.semanticui.elements.button.Button
import react.semanticui.elements.segment.Segment
import react.semanticui.elements.segment.SegmentGroup
import react.semanticui.sizes._

import scala.collection.immutable.SortedSet

final case class ConstraintGroupObsList(
  constraintsWithObs: View[ConstraintSummaryWithObervations],
  programId:          Program.Id,
  focusedObsSet:      Option[ObsIdSet],
  setSummaryPanel:    Reuse[Callback],
  expandedIds:        View[SortedSet[ObsIdSet]],
  undoStacks:         View[UndoStacks[IO, ConstraintGroupList]]
)(implicit val ctx:   AppContextIO)
    extends ReactProps[ConstraintGroupObsList](ConstraintGroupObsList.component)
    with ViewCommon

object ConstraintGroupObsList {
  type Props = ConstraintGroupObsList

  case class State(dragging: Boolean = false)
  object State {
    val dragging: Lens[State, Boolean] = Focus[State](_.dragging)
  }

  class Backend($ : BackendScope[Props, State]) {

    def toggleExpanded(
      obsIds:      ObsIdSet,
      expandedIds: View[SortedSet[ObsIdSet]]
    ): Callback =
      expandedIds.mod { expanded =>
        expanded.exists(_ === obsIds).fold(expanded - obsIds, expanded + obsIds)
      }

    /**
     * When we're dragging, we can have an observation id as the draggable id. If we have a
     * selection, and that id is part of the selection, we drag all the items in the selection.
     * However, the user may have something selected, but be dragging something that is NOT in the
     * selection - in which case we just drag the individual item.
     */
    def getDraggedIds(dragId: String, selected: Option[ObsIdSet]): Option[ObsIdSet] =
      Observation.Id.parse(dragId).map { dId =>
        val dIdSet = ObsIdSet.one(dId)
        selected.fold(dIdSet) { selectedIds =>
          if (selectedIds.contains(dId)) selectedIds
          else dIdSet
        }
      }

    def onDragEnd(
      undoCtx:     UndoContext[ConstraintGroupList],
      expandedIds: View[SortedSet[ObsIdSet]],
      setObsSet:   Option[ObsIdSet] => Callback
    )(implicit
      c:           TransactionalClient[IO, ObservationDB]
    ): (DropResult, ResponderProvided) => Callback = (result, _) =>
      $.props.flatMap { props =>
        val oData = for {
          destination <- result.destination.toOption
          destIds     <- ObsIdSet.fromString.getOption(destination.droppableId)
          draggedIds  <- getDraggedIds(result.draggableId, props.focusedObsSet)
          if !destIds.intersects(draggedIds)
          destCg      <- props.constraintsWithObs.get.constraintGroups.get(destIds)
        } yield (destCg, draggedIds)

        oData.foldMap { case (destCg, draggedIds) =>
          ConstraintGroupObsListActions
            .obsConstraintGroup(draggedIds, expandedIds, setObsSet)
            .set(undoCtx)(destCg.some)
        }
      }

    def render(props: Props) = {
      implicit val ctx = props.ctx

      val observations = props.constraintsWithObs.get.observations

      val constraintGroups = props.constraintsWithObs.get.constraintGroups.map(_._2)

      val state   = View.fromState($)
      val undoCtx = UndoContext(
        props.undoStacks,
        props.constraintsWithObs.zoom(ConstraintSummaryWithObervations.constraintGroups)
      )

      val renderClone: Draggable.Render = (provided, snapshot, rubric) =>
        <.div(provided.innerRef,
              provided.draggableProps,
              provided.dragHandleProps,
              props.getDraggedStyle(provided.draggableStyle, snapshot)
        )(
          getDraggedIds(rubric.draggableId, props.focusedObsSet)
            .flatMap(obsIds =>
              if (obsIds.size === 1)
                observations.get(obsIds.head).map(obs => props.renderObsBadge(obs))
              else {
                val div: TagMod = <.div(
                  SegmentGroup(
                    obsIds.toList.toTagMod(id => Segment(id.show))
                  )
                )
                div.some
              }
            )
            .getOrElse(<.span("ERROR"))
        )

      def isObsSelected(obsId: Observation.Id): Boolean =
        props.focusedObsSet.exists(_.contains(obsId))

      def setObsSet(obsIdSet: Option[ObsIdSet]): Callback =
        ctx.pushPage(AppTab.Constraints, props.programId, Focused(obsIdSet))

      def setObs(obsId: Observation.Id): Callback =
        setObsSet(ObsIdSet.one(obsId).some)

      val handleDragEnd = onDragEnd(undoCtx, props.expandedIds, setObsSet)

      def handleCtrlClick(obsId: Observation.Id, groupIds: ObsIdSet) =
        props.focusedObsSet.fold(setObs(obsId)) { selectedIds =>
          if (selectedIds.forall(groupIds.contains)) {
            if (selectedIds.contains(obsId)) {
              setObsSet(selectedIds.removeOne(obsId))
            } else setObsSet(selectedIds.add(obsId).some)
          } else Callback.empty // Not in the same group
        }

      def renderGroup(constraintGroup: ConstraintGroup): VdomNode = {
        val obsIds        = constraintGroup.obsIds
        val cgObs         = obsIds.toList.map(id => observations.get(id)).flatten
        // if this group or something in it is selected
        val groupSelected = props.focusedObsSet.exists(_.subsetOf(obsIds))

        val icon: FontAwesomeIcon = props.expandedIds.get
          .exists((ids: ObsIdSet) => ids === obsIds)
          .fold(Icons.ChevronDown, Icons.ChevronRight)
          .addModifiers(
            Seq(
              ^.cursor.pointer,
              ^.onClick ==> { e: ReactEvent =>
                e.stopPropagationCB >>
                  toggleExpanded(obsIds, props.expandedIds).asEventDefault(e).void
              }
            )
          )
          .fixedWidth()

        Droppable(ObsIdSet.fromString.reverseGet(obsIds), renderClone = renderClone) {
          case (provided, snapshot) =>
            val csHeader = <.span(ExploreStyles.ObsTreeGroupHeader)(
              icon,
              <.span(ExploreStyles.ObsGroupTitleWithWrap)(
                constraintGroup.constraintSet.shortName
              ),
              <.span(ExploreStyles.ObsCount, s"${obsIds.size} Obs")
            )

            <.div(
              provided.innerRef,
              provided.droppableProps,
              props.getListStyle(
                snapshot.draggingOverWith.exists(id => Observation.Id.parse(id).isDefined)
              )
            )(
              Segment(
                vertical = true,
                clazz = ExploreStyles.ObsTreeGroup |+| Option
                  .when(groupSelected)(ExploreStyles.SelectedObsTreeGroup)
                  .orElse(
                    Option.when(!state.get.dragging)(ExploreStyles.UnselectedObsTreeGroup)
                  )
                  .orEmpty
              )(^.cursor.pointer, ^.onClick --> setObsSet(constraintGroup.obsIds.some))(
                csHeader,
                TagMod.when(props.expandedIds.get.contains(obsIds))(
                  cgObs.zipWithIndex.toTagMod { case (obs, idx) =>
                    props.renderObsBadgeItem(
                      selectable = true,
                      highlightSelected = true,
                      forceHighlight = isObsSelected(obs.id),
                      linkToObsTab = false,
                      onSelect = setObs,
                      onCtrlClick = id => handleCtrlClick(id, obsIds)
                    )(obs, idx)
                  }
                ),
                provided.placeholder
              )
            )
        }
      }

      DragDropContext(
        onDragStart = (_: DragStart, _: ResponderProvided) => state.zoom(State.dragging).set(true),
        onDragEnd = (result, provided) =>
          state.zoom(State.dragging).set(false) >> handleDragEnd(result, provided)
      )(
        <.div(ExploreStyles.ObsTreeWrapper)(
          <.div(ExploreStyles.TreeToolbar)(UndoButtons(undoCtx, size = Mini)),
          <.div(
            Button(onClick = setObsSet(none) >> props.setSummaryPanel.value,
                   clazz = ExploreStyles.ButtonSummary
            )(
              Icons.ListIcon.clazz(ExploreStyles.PaddedRightIcon),
              "Constraints Summary"
            )
          ),
          <.div(ExploreStyles.ObsTree)(
            <.div(ExploreStyles.ObsScrollTree)(
              constraintGroups.toTagMod(renderGroup)
            )
          )
        )
      )
    }
  }

  protected val component = ScalaComponent
    .builder[Props]
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount { $ =>
      val constraintsWithObs = $.props.constraintsWithObs.get
      val constraintGroups   = constraintsWithObs.constraintGroups
      val expandedIds        = $.props.expandedIds

      val selectedGroup =
        $.props.focusedObsSet
          .flatMap(idSet => constraintGroups.find { case (key, _) => idSet.subsetOf(key) })
          .map(_._2)

      // Unfocus the group with observations doesn't exist
      val unfocus =
        if ($.props.focusedObsSet.nonEmpty && selectedGroup.isEmpty)
          $.props.ctx.replacePage(AppTab.Constraints, $.props.programId, Focused.None)
        else Callback.empty

      val expandSelected = selectedGroup.foldMap(cg => expandedIds.mod(_ + cg.obsIds))

      val cleanupExpandedIds =
        expandedIds.mod(_.filter(ids => constraintGroups.contains(ids)))

      for {
        _ <- unfocus
        _ <- expandSelected
        _ <- cleanupExpandedIds
      } yield ()
    }
    .build
}
