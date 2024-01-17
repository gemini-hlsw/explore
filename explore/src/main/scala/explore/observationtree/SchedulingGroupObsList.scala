// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Order.given
import cats.effect.IO
import cats.syntax.all.given
import clue.FetchClient
import crystal.react.View
import explore.Icons
import explore.common.TimingWindowsQueries
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.data.KeyedIndexedList
import explore.model.AppContext
import explore.model.Focused
import explore.model.ObsIdSet
import explore.model.ObsSummary
import explore.model.ObservationList
import explore.model.SchedulingGroupList
import explore.model.enums.AppTab
import explore.model.syntax.all.*
import explore.render.given
import explore.undo.UndoSetter
import explore.undo.Undoer
import explore.utils.ToastCtx
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.TimingWindowInclusion
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.TimingWindow
import lucuma.core.model.TimingWindowEnd
import lucuma.core.syntax.display.*
import lucuma.react.beautifuldnd.*
import lucuma.react.common.*
import lucuma.react.fa.FontAwesomeIcon
import lucuma.react.floatingui.syntax.*
import lucuma.schemas.ObservationDB
import lucuma.ui.primereact.*
import lucuma.ui.syntax.render.*
import lucuma.ui.utils.Render
import monocle.Iso
import mouse.boolean.*
import org.typelevel.log4cats.Logger

import scala.collection.immutable.SortedSet

case class SchedulingGroupObsList(
  programId:        Program.Id,
  observations:     UndoSetter[ObservationList],
  undoer:           Undoer,
  schedulingGroups: SchedulingGroupList,
  focusedObsSet:    Option[ObsIdSet],
  setSummaryPanel:  Callback,
  expandedIds:      View[SortedSet[ObsIdSet]]
) extends ReactFnProps[SchedulingGroupObsList](SchedulingGroupObsList.component)
    with ViewCommon

object SchedulingGroupObsList:
  private type Props = SchedulingGroupObsList

  private given Render[TimingWindowInclusion] = Render.by(twt =>
    <.span(twt match
      case TimingWindowInclusion.Include => ExploreStyles.TimingWindowInclude
      case TimingWindowInclusion.Exclude => ExploreStyles.TimingWindowExclude
    )(
      <.span(Icons.Circle).withTooltip(tooltip = twt.shortName)
    )
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
    FetchClient[IO, ObservationDB],
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
      .filterIndex((id: Observation.Id) =>
        oData.exists((_, _, draggedIds, _) => draggedIds.contains(id))
      )
      .andThen(KeyedIndexedList.value)
      .andThen(ObsSummary.timingWindows)

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
    .useEffectOnMountBy { (props, ctx, _) =>
      val expandedIds = props.expandedIds

      val selectedGroupObsIds =
        props.focusedObsSet
          .flatMap(idSet => props.schedulingGroups.find { case (key, _) => idSet.subsetOf(key) })
          .map(_._1)

      // Unfocus the group with observations doesn't exist
      val unfocus =
        if (props.focusedObsSet.nonEmpty && selectedGroupObsIds.isEmpty)
          ctx.replacePage(AppTab.Scheduling, props.programId, Focused.None)
        else Callback.empty

      val expandSelected = selectedGroupObsIds.foldMap(obsIds => expandedIds.mod(_ + obsIds))

      val cleanupExpandedIds =
        expandedIds.mod(_.filter(ids => props.schedulingGroups.contains(ids)))

      for {
        _ <- unfocus
        _ <- expandSelected
        _ <- cleanupExpandedIds
      } yield ()
    }
    .render { (props, ctx, dragging) =>
      import ctx.given

      val schedulingGroups = props.schedulingGroups.toList.sortBy(_._2.headOption)

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
        ctx.pushPage(AppTab.Scheduling, props.programId, Focused(obsIdSet))

      def setObs(obsId: Observation.Id): Callback =
        setObsSet(ObsIdSet.one(obsId).some)

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

        Droppable(ObsIdSet.fromString.reverseGet(obsIds), renderClone = renderClone) {
          case (provided, snapshot) =>
            val csHeader = <.span(ExploreStyles.ObsTreeGroupHeader)(
              icon,
              <.ul(ExploreStyles.ObsGroupTitleWithList)(
                timingWindows.map(tw => <.li(tw.renderVdom)).toTagMod
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
          <.div(ExploreStyles.TreeToolbar)(UndoButtons(props.undoer, size = PlSize.Mini)),
          <.div(ExploreStyles.ObsTree)(
            <.div(ExploreStyles.ObsScrollTree)(
              schedulingGroups.map((obsIds, c) => renderGroup(obsIds, c)).toTagMod
            )
          )
        )
      )
    }
