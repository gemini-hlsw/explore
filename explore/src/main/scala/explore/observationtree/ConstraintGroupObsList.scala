// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.react.View
import crystal.react.reuse.Reuse
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.model.AppContext
import explore.model.ConstraintGroup
import explore.model.Focused
import explore.model.ObsIdSet
import explore.model.display.given
import explore.model.enums.AppTab
import explore.undo.UndoContext
import explore.undo.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.syntax.all.*
import lucuma.schemas.ObservationDB
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import mouse.boolean.*
import react.beautifuldnd.*
import react.common.ReactFnProps
import react.fa.FontAwesomeIcon
import react.primereact.Button
import explore.common.AsterismQueries.ProgramSummaries
import explore.model.syntax.all.*

import scala.collection.immutable.SortedSet
import explore.common.AsterismQueries.ObservationList
import explore.common.AsterismQueries.ConstraintGroupList
import lucuma.core.model.ConstraintSet
import explore.common.ConstraintsQueries
import org.typelevel.log4cats.Logger
import lucuma.schemas.odb.input.*

case class ConstraintGroupObsList(
  programSummaries: View[ProgramSummaries],
  programId:        Program.Id,
  focusedObsSet:    Option[ObsIdSet],
  setSummaryPanel:  Callback,
  expandedIds:      View[SortedSet[ObsIdSet]],
  undoStacks:       View[UndoStacks[IO, ObservationList]]
) extends ReactFnProps[ConstraintGroupObsList](ConstraintGroupObsList.component)
    with ViewCommon

object ConstraintGroupObsList:
  private type Props = ConstraintGroupObsList

  private def toggleExpanded(obsIds: ObsIdSet, expandedIds: View[SortedSet[ObsIdSet]]): Callback =
    expandedIds.mod { expanded =>
      expanded.exists(_ === obsIds).fold(expanded - obsIds, expanded + obsIds)
    }

  /**
   * When we're dragging, we can have an observation id as the draggable id. If we have a selection,
   * and that id is part of the selection, we drag all the items in the selection. However, the user
   * may have something selected, but be dragging something that is NOT in the selection - in which
   * case we just drag the individual item.
   */
  private def getDraggedIds(dragId: String, selected: Option[ObsIdSet]): Option[ObsIdSet] =
    Observation.Id.parse(dragId).map { dId =>
      val dIdSet = ObsIdSet.one(dId)
      selected.fold(dIdSet) { selectedIds =>
        if (selectedIds.contains(dId)) selectedIds
        else dIdSet
      }
    }

  private def onDragEnd(
    undoCtx:          UndoContext[ObservationList],
    programId:        Program.Id,
    expandedIds:      View[SortedSet[ObsIdSet]],
    focusedObsSet:    Option[ObsIdSet],
    constraintGroups: ConstraintGroupList,
    setObsSet:        Option[ObsIdSet] => Callback
  )(using
    FetchClient[IO, ?, ObservationDB],
    Logger[IO]
  ): (DropResult, ResponderProvided) => Callback = (result, _) => {
    val oData = for {
      destination <- result.destination.toOption
      destIds     <- ObsIdSet.fromString.getOption(destination.droppableId)
      draggedIds  <- getDraggedIds(result.draggableId, focusedObsSet)
      if !destIds.intersects(draggedIds)
      newCs       <- constraintGroups.get(destIds)
    } yield (newCs, draggedIds)

    oData.foldMap { case (newCs, draggedIds) =>
      ConstraintsQueries
        .UndoView(programId, draggedIds, undoCtx)(
          // There should be a better way to do this.
          identity,
          identity,
          cs => _ => cs.toInput
        )
        .set(newCs)
    }

    // Oh, this will be fun... we have to change the whole approach to edit observations.
    // We will probably need a Traversal

    // oData.foldMap { case (destCg, draggedIds) =>
    //   ConstraintGroupObsListActions
    //     .obsConstraintGroup(programId, draggedIds, expandedIds, setObsSet)
    //     .set(undoCtx)(destCg.some)
    // }
  }

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useState(false) // dragging
    .useEffectOnMountBy { (props, ctx, _) =>
      val programSummaries = props.programSummaries.get
      val constraintGroups =
        programSummaries.constraintGroups // SortedMap[ObsIdSet, ConstraintSet]
      val expandedIds = props.expandedIds

      val selectedGroupObsIds =
        props.focusedObsSet
          .flatMap(idSet => constraintGroups.find { case (key, _) => idSet.subsetOf(key) })
          .map(_._1)

      // Unfocus the group with observations doesn't exist
      val unfocus =
        if (props.focusedObsSet.nonEmpty && selectedGroupObsIds.isEmpty)
          ctx.replacePage(AppTab.Constraints, props.programId, Focused.None)
        else Callback.empty

      val expandSelected = selectedGroupObsIds.foldMap(obsIds => expandedIds.mod(_ + obsIds))

      val cleanupExpandedIds =
        expandedIds.mod(_.filter(ids => constraintGroups.contains(ids)))

      for {
        _ <- unfocus
        _ <- expandSelected
        _ <- cleanupExpandedIds
      } yield ()
    }
    .render { (props, ctx, dragging) =>
      import ctx.given

      val observations = props.programSummaries.get.observations

      val constraintGroups =
        props.programSummaries.get.constraintGroups.toList.sortBy(_._2.summaryString)

      val undoCtx = UndoContext(
        props.undoStacks,
        props.programSummaries.zoom(ProgramSummaries.observations)
      )

      val renderClone: Draggable.Render = (provided, snapshot, rubric) =>
        <.div(
          provided.innerRef,
          provided.draggableProps,
          provided.dragHandleProps,
          props.getDraggedStyle(provided.draggableStyle, snapshot)
        )(
          getDraggedIds(rubric.draggableId, props.focusedObsSet)
            .flatMap(obsIds =>
              if (obsIds.size === 1)
                observations
                  .getValue(obsIds.head)
                  .map(obs => props.renderObsBadge(obs, ObsBadge.Layout.ConstraintsTab))
              else
                <.div(obsIds.toList.toTagMod(id => <.div(id.show))).some
            )
            .getOrElse(<.span("ERROR"))
        )

      def isObsSelected(obsId: Observation.Id): Boolean =
        props.focusedObsSet.exists(_.contains(obsId))

      def setObsSet(obsIdSet: Option[ObsIdSet]): Callback =
        ctx.pushPage(AppTab.Constraints, props.programId, Focused(obsIdSet))

      def setObs(obsId: Observation.Id): Callback =
        setObsSet(ObsIdSet.one(obsId).some)

      val handleDragEnd = onDragEnd(
        undoCtx,
        props.programId,
        props.expandedIds,
        props.focusedObsSet,
        props.programSummaries.get.constraintGroups,
        setObsSet
      )

      def handleCtrlClick(obsId: Observation.Id, groupIds: ObsIdSet) =
        props.focusedObsSet.fold(setObs(obsId)) { selectedIds =>
          if (selectedIds.forall(groupIds.contains)) {
            if (selectedIds.contains(obsId)) {
              setObsSet(selectedIds.removeOne(obsId))
            } else setObsSet(selectedIds.add(obsId).some)
          } else Callback.empty // Not in the same group
        }

      def renderGroup(obsIds: ObsIdSet, constraintSet: ConstraintSet): VdomNode = {
        val cgObs         = obsIds.toList.map(id => observations.getValue(id)).flatten
        // if this group or something in it is selected
        val groupSelected = props.focusedObsSet.exists(_.subsetOf(obsIds))

        val icon: FontAwesomeIcon = props.expandedIds.get
          .exists((ids: ObsIdSet) => ids === obsIds)
          .fold(Icons.ChevronDown, Icons.ChevronRight)
          .addModifiers(
            Seq(
              ^.cursor.pointer,
              ^.onClick ==> { (e: ReactEvent) =>
                e.stopPropagationCB >>
                  toggleExpanded(obsIds, props.expandedIds).asEventDefault(e).void
              }
            )
          )
          .withFixedWidth()

        Droppable(ObsIdSet.fromString.reverseGet(obsIds), renderClone = renderClone) {
          case (provided, snapshot) =>
            val csHeader = <.span(ExploreStyles.ObsTreeGroupHeader)(
              icon,
              <.span(ExploreStyles.ObsGroupTitleWithWrap)(constraintSet.shortName),
              <.span(ExploreStyles.ObsCount, s"${obsIds.size} Obs")
            )

            <.div(
              provided.innerRef,
              provided.droppableProps,
              props.getListStyle(
                snapshot.draggingOverWith.exists(id => Observation.Id.parse(id).isDefined)
              )
            )(
              <.div(
                ExploreStyles.ObsTreeGroup |+| Option
                  .when(groupSelected)(ExploreStyles.SelectedObsTreeGroup)
                  .orElse(
                    Option.when(!dragging.value)(ExploreStyles.UnselectedObsTreeGroup)
                  )
                  .orEmpty
              )(^.cursor.pointer, ^.onClick --> setObsSet(obsIds.some))(
                csHeader,
                TagMod.when(props.expandedIds.get.contains(obsIds))(
                  cgObs.zipWithIndex.toTagMod { case (obs, idx) =>
                    props.renderObsBadgeItem(
                      ObsBadge.Layout.ConstraintsTab,
                      selectable = true,
                      highlightSelected = true,
                      forceHighlight = isObsSelected(obs.id),
                      linkToObsTab = false,
                      onSelect = setObs,
                      onCtrlClick = id => handleCtrlClick(id, obsIds),
                      ctx
                    )(obs, idx)
                  }
                ),
                provided.placeholder
              )
            )
        }
      }

      DragDropContext(
        onDragStart = (_: DragStart, _: ResponderProvided) => dragging.setState(true),
        onDragEnd =
          (result, provided) => dragging.setState(false) >> handleDragEnd(result, provided)
      )(
        <.div(ExploreStyles.ObsTreeWrapper)(
          <.div(ExploreStyles.TreeToolbar)(UndoButtons(undoCtx, size = PlSize.Mini)),
          <.div(
            Button(
              onClick = setObsSet(none) >> props.setSummaryPanel,
              clazz = ExploreStyles.ButtonSummary,
              severity = Button.Severity.Secondary
            )(
              Icons.ListIcon.withClass(ExploreStyles.PaddedRightIcon),
              "Constraints Summary"
            )
          ),
          <.div(ExploreStyles.ObsTree)(
            <.div(ExploreStyles.ObsScrollTree)(
              constraintGroups.map((obsIds, c) => renderGroup(obsIds, c)).toTagMod
            )
          )
        )
      )
    }
