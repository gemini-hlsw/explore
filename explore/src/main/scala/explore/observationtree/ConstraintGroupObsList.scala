// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import explore.Icons
import explore.common.ConstraintsQueries
import explore.components.ActionButtons
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.model.AppContext
import explore.model.ConstraintGroupList
import explore.model.Focused
import explore.model.ObsIdSet
import explore.model.ObsIdSetEditInfo
import explore.model.Observation
import explore.model.ObservationList
import explore.model.display.given
import explore.model.enums.AppTab
import explore.model.syntax.all.*
import explore.services.OdbObservationApi
import explore.undo.*
import explore.utils.*
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
import lucuma.react.primereact.Message
import lucuma.schemas.odb.input.*
import lucuma.ui.primereact.*
import lucuma.ui.syntax.all.given
import monocle.Iso
import mouse.boolean.*
import org.typelevel.log4cats.Logger

import scala.collection.immutable.SortedSet

case class ConstraintGroupObsList(
  programId:             Program.Id,
  observations:          UndoSetter[ObservationList],
  undoer:                Undoer,
  constraintGroups:      ConstraintGroupList,
  focusedObsSet:         Option[ObsIdSet],
  setSummaryPanel:       Callback,
  expandedIds:           View[SortedSet[ObsIdSet]],
  copyCallback:          Callback,
  pasteCallback:         Callback,
  clipboardObsContents:  Option[ObsIdSet],
  allocatedScienceBands: SortedSet[ScienceBand],
  readonly:              Boolean
) extends ReactFnProps[ConstraintGroupObsList](ConstraintGroupObsList.component)
    with ViewCommon:
  private val copyDisabled: Boolean                                    = focusedObsSet.isEmpty
  private val pasteDisabled: Boolean                                   = clipboardObsContents.isEmpty
  private val (deleteDisabled: Boolean, deleteTooltip: Option[String]) =
    focusedObsSet.fold((true, none))(obsIds =>
      if (observations.get.executedOf(obsIds).nonEmpty)
        (true, "- Cannot delete executed observations.".some)
      else (false, none)
    )

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
        .get(obsIdSet.head) // All focused obs have the same constraints, so we can use head
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
    OdbObservationApi[IO],
    Logger[IO],
    ToastCtx[IO]
  ): (DropResult, ResponderProvided) => Callback = (result, _) =>
    (for
      destination <- result.destination.toOption
      destIds     <- ObsIdSet.fromString.getOption(destination.droppableId)
      draggedIds  <- getDraggedIds(result.draggableId, focusedObsSet)
      if !destIds.intersects(draggedIds)
      newCs       <- constraintGroups.get(destIds)
      grp         <- constraintGroups.findContainingObsIds(draggedIds)
    yield
      val obsEditInfo = ObsIdSetEditInfo.fromObservationList(draggedIds, observations.get)

      if (obsEditInfo.executed.nonEmpty)
        ToastCtx[IO]
          .showToast(
            "Cannot modify constraints for executed observations.",
            Message.Severity.Error,
            true
          )
          .runAsync
      else
        val traversal = Iso
          .id[ObservationList]
          .filterIndex(draggedIds.contains)
          .andThen(Observation.constraints)

        val constraintSet =
          observations.zoom(traversal.getAll.andThen(_.head), traversal.modify)

        expandedIds.mod(ids =>
          val base = ids - draggedIds - destIds + (destIds ++ draggedIds)
          (grp.obsIds -- draggedIds).fold(base)(base + _)
        ) >>
          ConstraintsQueries
            .UndoView(draggedIds, constraintSet)(
              // There should be a better way to do this.
              identity,
              identity,
              cs => _ => cs.toInput
            )
            .set(newCs)
    ).orEmpty

  private val component = ScalaFnComponent[Props]: props =>
    for
      ctx      <- useContext(AppContext.ctx)
      dragging <- useState(false)
      _        <- useEffectOnMount:
                    val expandedIds = props.expandedIds

                    val selectedGroupObsIds =
                      props.focusedObsSet
                        .flatMap(idSet =>
                          props.constraintGroups.find { case (key, _) => idSet.subsetOf(key) }
                        )
                        .map(_._1)

                    // Unfocus the group with observations doesn't exist
                    val unfocus =
                      if (props.focusedObsSet.nonEmpty && selectedGroupObsIds.isEmpty)
                        ctx.replacePage((AppTab.Constraints, props.programId, Focused.None).some)
                      else Callback.empty

                    val expandSelected = selectedGroupObsIds.foldMap(obsIds => expandedIds.mod(_ + obsIds))

                    val cleanupExpandedIds =
                      expandedIds.mod(_.filter(ids => props.constraintGroups.contains(ids)))

                    for
                      _ <- unfocus
                      _ <- expandSelected
                      _ <- cleanupExpandedIds
                    yield ()
    yield
      import ctx.given

      val constraintGroups: List[(ObsIdSet, ConstraintSet)] =
        props.constraintGroups.toList.sortBy(_._2.summaryString)

      val renderClone: Draggable.Render = (provided, snapshot, rubric) =>
        <.div(
          provided.innerRef,
          provided.draggableProps,
          provided.dragHandleProps,
          props.getDraggedStyle(provided.draggableStyle, snapshot)
        )(
          getDraggedIds(rubric.draggableId, props.focusedObsSet)
            .flatMap(obsIds =>
              val obsEditInfo = ObsIdSetEditInfo.fromObservationList(obsIds, props.observations.get)

              if (obsEditInfo.executed.nonEmpty)
                val m: TagMod =
                  Message(
                    text = "Contains executed observations",
                    severity = Message.Severity.Error,
                    icon = Icons.ErrorIcon
                  )
                m.some
              else if (obsIds.size === 1)
                props.observations.get
                  .get(obsIds.head)
                  .map(obs => props.renderObsBadge(obs, ObsBadge.Layout.ConstraintsTab))
              else
                <.div(obsIds.toList.toTagMod(using id => <.div(id.show))).some
            )
            .getOrElse(<.span("ERROR"))
        )

      def isObsSelected(obsId: Observation.Id): Boolean =
        props.focusedObsSet.exists(_.contains(obsId))

      def setObsSet(obsIdSet: Option[ObsIdSet]): Callback =
        ctx.pushPage((AppTab.Constraints, props.programId, Focused(obsIdSet)).some)

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

      val deleteObs: ObsIdSet => Callback = obsIds =>
        props.constraintGroups.keys
          .find(k => obsIds.subsetOf(k))
          .foldMap: groupObsIds =>
            props.undoableDeleteObs(
              obsIds,
              props.observations, {
                // After deletion keep expanded group
                val newObsIds = groupObsIds -- obsIds
                val expansion =
                  newObsIds.fold(Callback.empty)(a => props.expandedIds.mod(_ + a))
                expansion *> setObsSet(newObsIds)
              }
            )

      def renderGroup(obsIds: ObsIdSet, constraintSet: ConstraintSet): VdomNode = {
        val cgObs         = obsIds.toList.map(id => props.observations.get.get(id)).flatten
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

          def badgeItem(obs: Observation, idx: Int): TagMod =
            props.renderObsBadgeItem(
              ObsBadge.Layout.ConstraintsTab,
              selectable = true,
              highlightSelected = true,
              forceHighlight = isObsSelected(obs.id),
              linkToObsTab = false,
              onSelect = setObs,
              onDelete = deleteObs(ObsIdSet.one(obs.id)),
              onCtrlClick = id => handleCtrlClick(id, obsIds),
              ctx = ctx
            )(obs, idx)

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
                cgObs.zipWithIndex.toTagMod(using badgeItem)
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
                props.focusedObsSet.foldMap(deleteObs),
                disabled = props.deleteDisabled,
                tooltipExtra = props.deleteTooltip.orElse(props.selectedText)
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
