// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.react.View
import explore.Icons
import explore.common.ConstraintsQueries
import explore.components.ActionButtons
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.data.KeyedIndexedList
import explore.model.AppContext
import explore.model.ConstraintGroupList
import explore.model.Focused
import explore.model.ObsIdSet
import explore.model.Observation
import explore.model.ObservationExecutionMap
import explore.model.ObservationList
import explore.model.display.given
import explore.model.enums.AppTab
import explore.model.syntax.all.*
import explore.undo.*
import explore.utils.*
import explore.utils.ToastCtx
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ScienceBand
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Program
import lucuma.core.syntax.all.*
import lucuma.react.beautifuldnd.*
import lucuma.react.common.ReactFnProps
import lucuma.react.fa.FontAwesomeIcon
import lucuma.react.primereact.Button
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.input.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given
import monocle.Iso
import mouse.boolean.*
import org.typelevel.log4cats.Logger

import scala.collection.immutable.SortedSet

case class ConstraintGroupObsList(
  programId:               Program.Id,
  observations:            UndoSetter[ObservationList],
  undoer:                  Undoer,
  constraintGroups:        ConstraintGroupList,
  calibrationObservations: Set[Observation.Id],
  obsExecutions:           ObservationExecutionMap,
  focusedObsSet:           Option[ObsIdSet],
  setSummaryPanel:         Callback,
  expandedIds:             View[SortedSet[ObsIdSet]],
  copyCallback:            Callback,
  pasteCallback:           Callback,
  clipboardObsContents:    Option[ObsIdSet],
  allocatedScienceBands:   SortedSet[ScienceBand],
  readonly:                Boolean
) extends ReactFnProps[ConstraintGroupObsList](ConstraintGroupObsList.component)
    with ViewCommon:
  private val copyDisabled: Boolean  = focusedObsSet.isEmpty
  private val pasteDisabled: Boolean = clipboardObsContents.isEmpty
  private val deleteDisabled: Boolean =
    // For now, we only allow deleting when just one obs is selected
    !focusedObsSet.exists(_.size === 1)

  private def observationsText(observations: ObsIdSet): String =
    observations.idSet.size match
      case 1    => s"observation ${observations.idSet.head}"
      case more => s"$more observations"
  private def constraintSetText(cs: ConstraintSet): String     = s"constraints '${cs.shortName}'"

  private val selectedText: Option[String]  = focusedObsSet.map(observationsText)
  private val clipboardText: Option[String] = clipboardObsContents.map(observationsText)
  private val pasteIntoText: Option[String] =
    focusedObsSet.flatMap: obsIdSet =>
      observations.get
        .getValue(obsIdSet.head) // All focused obs have the same constraints, so we can use head
        .map(obs => constraintSetText(obs.constraints))
  private val pasteText: Option[String]     =
    Option
      .unless(pasteDisabled)((clipboardText, pasteIntoText).mapN((c, s) => s"$c into $s"))
      .flatten

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
    observations:     UndoSetter[ObservationList],
    expandedIds:      View[SortedSet[ObsIdSet]],
    focusedObsSet:    Option[ObsIdSet],
    constraintGroups: ConstraintGroupList
  )(using
    FetchClient[IO, ObservationDB],
    Logger[IO],
    ToastCtx[IO]
  ): (DropResult, ResponderProvided) => Callback = (result, _) => {
    val oData = for {
      destination <- result.destination.toOption
      destIds     <- ObsIdSet.fromString.getOption(destination.droppableId)
      draggedIds  <- getDraggedIds(result.draggableId, focusedObsSet)
      if !destIds.intersects(draggedIds)
      newCs       <- constraintGroups.get(destIds)
      srcIds      <- constraintGroups.findContainingObsIds(draggedIds)
    } yield (newCs, destIds, draggedIds, srcIds.obsIds)

    val traversal = Iso
      .id[ObservationList]
      .filterIndex((id: Observation.Id) =>
        oData.exists((_, _, draggedIds, _) => draggedIds.contains(id))
      )
      .andThen(KeyedIndexedList.value)
      .andThen(Observation.constraints)

    val constraintSet =
      observations.zoom(traversal.getAll.andThen(_.head), traversal.modify)

    oData.foldMap { case (newCs, destIds, draggedIds, srcIds) =>
      expandedIds.mod(ids =>
        val base = ids - draggedIds - destIds + (destIds ++ draggedIds)
        (srcIds -- draggedIds).fold(base)(base + _)
      ) >>
        ConstraintsQueries
          .UndoView(draggedIds, constraintSet)(
            // There should be a better way to do this.
            identity,
            identity,
            cs => _ => cs.toInput
          )
          .set(newCs)
    }
  }

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useState(false) // dragging
    .useEffectOnMountBy: (props, ctx, _) =>
      val expandedIds = props.expandedIds

      val selectedGroupObsIds =
        props.focusedObsSet
          .flatMap(idSet => props.constraintGroups.find { case (key, _) => idSet.subsetOf(key) })
          .map(_._1)

      // Unfocus the group with observations doesn't exist
      val unfocus =
        if (props.focusedObsSet.nonEmpty && selectedGroupObsIds.isEmpty)
          ctx.replacePage(AppTab.Constraints, props.programId, Focused.None)
        else Callback.empty

      val expandSelected = selectedGroupObsIds.foldMap(obsIds => expandedIds.mod(_ + obsIds))

      val cleanupExpandedIds =
        expandedIds.mod(_.filter(ids => props.constraintGroups.contains(ids)))

      for {
        _ <- unfocus
        _ <- expandSelected
        _ <- cleanupExpandedIds
      } yield ()
    .render: (props, ctx, dragging) =>
      import ctx.given

      val constraintGroups: List[(ObsIdSet, ConstraintSet)] =
        props.constraintGroups
          .map: (obsIdSet, constraintGroups) =>
            (obsIdSet -- props.calibrationObservations).map: filteredObsIdSet =>
              (filteredObsIdSet, constraintGroups)
          .toList
          .flattenOption
          .sortBy(_._2.summaryString)

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
                props.observations.get
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
        props.observations,
        props.expandedIds,
        props.focusedObsSet,
        props.constraintGroups
      )

      def handleCtrlClick(obsId: Observation.Id, groupIds: ObsIdSet) =
        props.focusedObsSet.fold(setObs(obsId)) { selectedIds =>
          if (selectedIds.forall(groupIds.contains)) {
            if (selectedIds.contains(obsId)) {
              setObsSet(selectedIds.removeOne(obsId))
            } else setObsSet(selectedIds.add(obsId).some)
          } else Callback.empty // Not in the same group
        }

      val deleteObs: Observation.Id => Callback = obsId =>
        props.constraintGroups.keys
          .find(_.contains(obsId))
          .foldMap: obsIds =>
            props.undoableDeleteObs(
              obsId,
              props.observations,
              o => setObsSet(obsIds.add(o).some), {
                // After deletion keep expanded group
                val newObsIds = obsIds - obsId
                val expansion =
                  newObsIds.fold(Callback.empty)(a => props.expandedIds.mod(_ + a))
                expansion *> setObsSet(newObsIds)
              }
            )

      def renderGroup(obsIds: ObsIdSet, constraintSet: ConstraintSet): VdomNode = {
        val cgObs         = obsIds.toList.map(id => props.observations.get.getValue(id)).flatten
        // if this group or something in it is selected
        val groupSelected = props.focusedObsSet.exists(_.subsetOf(obsIds))

        val icon: FontAwesomeIcon = props.expandedIds.get
          .exists((ids: ObsIdSet) => ids === obsIds)
          .fold(Icons.ChevronDown, Icons.ChevronRight)(
            ^.cursor.pointer,
            ^.onClick ==> { (e: ReactEvent) =>
              e.stopPropagationCB >>
                toggleExpanded(obsIds, props.expandedIds).asEventDefault(e).void
            }
          )
          .withFixedWidth()

        Droppable(
          ObsIdSet.fromString.reverseGet(obsIds),
          renderClone = renderClone,
          isDropDisabled = props.readonly
        ) { case (provided, snapshot) =>
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
                    onDelete = deleteObs(obs.id),
                    onCtrlClick = id => handleCtrlClick(id, obsIds),
                    ctx = ctx
                  )(obs, idx)
                }
              ),
              provided.placeholder
            )
          )
        }
      }

      DragDropContext(
        onDragStart =
          (_: DragStart, _: ResponderProvided) => dragging.setState(true).unless_(props.readonly),
        onDragEnd = (result, provided) =>
          (dragging.setState(false) >> handleDragEnd(result, provided)).unless_(props.readonly)
      )(
        <.div(ExploreStyles.ObsTreeWrapper)(
          <.div(ExploreStyles.TreeToolbar)(
            UndoButtons(props.undoer, size = PlSize.Mini),
            ActionButtons(
              ActionButtons.ButtonProps(
                props.copyCallback,
                disabled = props.copyDisabled,
                tooltipExtra = props.selectedText
              ),
              ActionButtons.ButtonProps(
                props.pasteCallback,
                disabled = props.pasteDisabled,
                tooltipExtra = props.pasteText
              ),
              ActionButtons.ButtonProps(
                props.focusedObsSet.foldMap(obsIdSet => deleteObs(obsIdSet.head)),
                disabled = props.deleteDisabled,
                tooltipExtra = props.selectedText
              )
            )
          )
            .unless(props.readonly),
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
