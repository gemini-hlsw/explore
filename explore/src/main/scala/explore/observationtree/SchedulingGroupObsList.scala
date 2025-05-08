// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Order.given
import cats.effect.IO
import cats.syntax.all.given
import crystal.react.View
import explore.Icons
import explore.common.TimingWindowsQueries
import explore.components.ActionButtons
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.model.AppContext
import explore.model.Focused
import explore.model.ObsIdSet
import explore.model.Observation
import explore.model.ObservationExecutionMap
import explore.model.ObservationList
import explore.model.SchedulingGroupList
import explore.model.enums.AppTab
import explore.model.syntax.all.*
import explore.render.given
import explore.services.OdbObservationApi
import explore.undo.UndoSetter
import explore.undo.Undoer
import explore.utils.ToastCtx
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimingWindowInclusion
import lucuma.core.model.Program
import lucuma.core.model.TimingWindow
import lucuma.core.model.TimingWindowEnd
import lucuma.core.syntax.display.*
import lucuma.react.beautifuldnd.*
import lucuma.react.common.*
import lucuma.react.fa.FontAwesomeIcon
import lucuma.react.floatingui.syntax.*
import lucuma.ui.primereact.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.render.*
import lucuma.ui.utils.Render
import monocle.Iso
import mouse.boolean.*
import org.typelevel.log4cats.Logger

import scala.collection.immutable.SortedSet

case class SchedulingGroupObsList(
  programId:             Program.Id,
  observations:          UndoSetter[ObservationList],
  undoer:                Undoer,
  schedulingGroups:      SchedulingGroupList,
  obsExecutions:         ObservationExecutionMap,
  focusedObsSet:         Option[ObsIdSet],
  setSummaryPanel:       Callback,
  expandedIds:           View[SortedSet[ObsIdSet]],
  copyCallback:          Callback,
  pasteCallback:         Callback,
  clipboardObsContents:  Option[ObsIdSet],
  allocatedScienceBands: SortedSet[ScienceBand],
  readonly:              Boolean
) extends ReactFnProps[SchedulingGroupObsList](SchedulingGroupObsList.component)
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

  private val selectedText: Option[String]  = focusedObsSet.map(observationsText)
  private val clipboardText: Option[String] = clipboardObsContents.map(observationsText)
  private val pasteIntoText: Option[String] =
    focusedObsSet.as("active scheduling group")
  private val pasteText: Option[String]     =
    Option
      .unless(pasteDisabled)((clipboardText, pasteIntoText).mapN((c, s) => s"$c into $s"))
      .flatten

object SchedulingGroupObsList:
  private type Props = SchedulingGroupObsList

  private given Render[TimingWindowInclusion] = Render.by: twt =>
    <.span(twt match
      case TimingWindowInclusion.Include => ExploreStyles.TimingWindowInclude
      case TimingWindowInclusion.Exclude => ExploreStyles.TimingWindowExclude
    )(
      <.span(Icons.CircleSolid).withTooltip(tooltip = twt.shortName)
    )

  private given Render[Option[TimingWindowEnd]] = Render.by {
    case None                                             =>
      <.span(Icons.ArrowRight).withTooltip(tooltip = "forever")
    case Some(TimingWindowEnd.At(endUtc))                 =>
      <.span(Icons.ArrowRightToLine).withTooltip(tooltip = s"through ${endUtc.formatUtc}")
    case Some(after @ TimingWindowEnd.After(ts, None))    =>
      <.span(Icons.ArrowRightToLine).withTooltip(tooltip = after.renderVdom)
    case Some(after @ TimingWindowEnd.After(ts, Some(_))) =>
      <.span(Icons.ArrowsRepeat).withTooltip(tooltip = after.renderVdom)
  }

  private given Render[TimingWindow] = Render.by { case tw @ TimingWindow(inclusion, start, end) =>
    React.Fragment(
      inclusion.renderVdom,
      " ",
      start.formatUtc,
      " ",
      end.renderVdom
    )
  }

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
    undoCtx:          UndoSetter[ObservationList],
    expandedIds:      View[SortedSet[ObsIdSet]],
    focusedObsSet:    Option[ObsIdSet],
    schedulingGroups: SchedulingGroupList
  )(using
    OdbObservationApi[IO],
    Logger[IO],
    ToastCtx[IO]
  ): (DropResult, ResponderProvided) => Callback = (result, _) => {
    val oData = for {
      destination <- result.destination.toOption
      destIds     <- ObsIdSet.fromString.getOption(destination.droppableId)
      draggedIds  <- getDraggedIds(result.draggableId, focusedObsSet)
      if !destIds.intersects(draggedIds)
      newTw       <- schedulingGroups.get(destIds)
      srcIds      <- schedulingGroups.findContainingObsIds(draggedIds)
    } yield (newTw, destIds, draggedIds, srcIds.obsIds)

    val traversal = Iso
      .id[ObservationList]
      .filterIndex: (id: Observation.Id) =>
        oData.exists((_, _, draggedIds, _) => draggedIds.contains(id))
      .andThen(Observation.timingWindows)

    val twUndoCtx =
      undoCtx.zoom(traversal.getAll.andThen(_.head), traversal.modify)

    oData.foldMap { case (newTw, destIds, draggedIds, srcIds) =>
      expandedIds.mod(ids =>
        val base = ids - draggedIds - destIds + (destIds ++ draggedIds)
        (srcIds -- draggedIds).fold(base)(base + _)
      ) >>
        TimingWindowsQueries
          .viewWithRemoteMod[IO](
            draggedIds,
            twUndoCtx.undoableView(Iso.id.asLens)
          )
          .set(newTw)
    }
  }

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useState(false) // dragging
    .useEffectWithDepsBy((props, _, _) => props.schedulingGroups): (props, ctx, _) =>
      schedulingGroups =>
        val expandedIds: View[SortedSet[ObsIdSet]] = props.expandedIds

        val selectedGroupObsIds: Option[ObsIdSet] =
          props.focusedObsSet
            .flatMap(idSet => schedulingGroups.find { case (key, _) => idSet.subsetOf(key) })
            .map(_._1)

        // Unfocus the group with observations that doesn't exist
        val unfocus: Callback =
          if (props.focusedObsSet.nonEmpty && selectedGroupObsIds.isEmpty)
            ctx.replacePage((AppTab.Scheduling, props.programId, Focused.None).some)
          else Callback.empty

        val expandSelected: Callback =
          selectedGroupObsIds.foldMap(obsIds => expandedIds.mod(_ + obsIds))

        val cleanupExpandedIds: Callback =
          expandedIds.mod:
            _.map: obsIdSet =>
              schedulingGroups.keys.find(_.intersect(obsIdSet).nonEmpty)
            .collect { case Some(obsIdSet) => obsIdSet }

        unfocus >> expandSelected >> cleanupExpandedIds
    .render: (props, ctx, dragging) =>
      import ctx.given

      val schedulingGroups: List[(ObsIdSet, List[TimingWindow])] =
        props.schedulingGroups.toList.sortBy(_._2.headOption)

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
        ctx.pushPage((AppTab.Scheduling, props.programId, Focused(obsIdSet)).some)

      def setObs(obsId: Observation.Id): Callback =
        setObsSet(ObsIdSet.one(obsId).some)

      val deleteObs: Observation.Id => Callback = obsId =>
        props.schedulingGroups.keys
          .find(_.contains(obsId))
          .foldMap: obsIds =>
            props.undoableDeleteObs(
              obsId,
              props.observations, {
                // After deletion expanded group
                val newObsIds = obsIds - obsId
                val expansion =
                  newObsIds.fold(Callback.empty)(a => props.expandedIds.mod(_ + a))
                expansion *> setObsSet(newObsIds)
              }
            )

      val handleDragEnd = onDragEnd(
        props.observations,
        props.expandedIds,
        props.focusedObsSet,
        props.schedulingGroups
      )

      def handleCtrlClick(obsId: Observation.Id, groupIds: ObsIdSet) =
        props.focusedObsSet.fold(setObs(obsId)) { selectedIds =>
          if (selectedIds.forall(groupIds.contains)) {
            if (selectedIds.contains(obsId)) {
              setObsSet(selectedIds.removeOne(obsId))
            } else setObsSet(selectedIds.add(obsId).some)
          } else Callback.empty // Not in the same group
        }

      def renderGroup(obsIds: ObsIdSet, timingWindows: List[TimingWindow]): VdomNode = {
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

        Droppable(ObsIdSet.fromString.reverseGet(obsIds),
                  renderClone = renderClone,
                  isDropDisabled = props.readonly
        ) { case (provided, snapshot) =>
          val csHeader = <.span(ExploreStyles.ObsTreeGroupHeader)(
            icon,
            <.ul(ExploreStyles.ObsGroupTitleWithList)(
              timingWindows.map(tw => <.li(tw.renderVdom)).toTagMod
            ),
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
              onDelete = deleteObs(obs.id),
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
                .orElse:
                  Option.when(!dragging.value)(ExploreStyles.UnselectedObsTreeGroup)
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
                props.focusedObsSet.foldMap(obsIdSet => deleteObs(obsIdSet.head)),
                disabled = props.deleteDisabled,
                tooltipExtra = props.selectedText
              )
            )
          )
            .unless(props.readonly),
          <.div(ExploreStyles.ObsTree)(
            <.div(ExploreStyles.ObsScrollTree)(
              schedulingGroups.map((obsIds, c) => renderGroup(obsIds, c)).toTagMod
            )
          )
        )
      )
