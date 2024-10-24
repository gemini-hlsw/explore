// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.react.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.ObsIdSet
import explore.model.Observation
import explore.model.ObservationExecutionMap
import explore.model.ObservationList
import explore.undo.UndoSetter
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.ScienceBand
import lucuma.core.model.Program
import lucuma.react.beautifuldnd.*
import lucuma.schemas.ObservationDB
import lucuma.ui.syntax.all.given
import org.typelevel.log4cats.Logger

import scala.collection.immutable.SortedSet

trait ViewCommon {
  def programId: Program.Id
  def focusedObsSet: Option[ObsIdSet]
  def obsExecutions: ObservationExecutionMap
  def allocatedScienceBands: SortedSet[ScienceBand]
  def readonly: Boolean

  def renderObsBadge(
    obs:               Observation,
    layout:            ObsBadge.Layout,
    highlightSelected: Boolean = true,
    forceHighlight:    Boolean = false, // if true, overrides highlightSelected
    onDelete:          Callback = Callback.empty
  ): TagMod =
    ObsBadge(
      obs,
      obsExecutions.getPot(obs.id).map(_.programTimeEstimate),
      layout,
      selected = forceHighlight || (highlightSelected && focusedObsSet.exists(_.contains(obs.id))),
      readonly = readonly,
      deleteCB = onDelete,
      allocatedScienceBands = allocatedScienceBands
    )

  def renderObsBadgeItem(
    layout:            ObsBadge.Layout,
    selectable:        Boolean,
    onSelect:          Observation.Id => Callback,
    onDelete:          Callback,
    highlightSelected: Boolean = true,
    forceHighlight:    Boolean = false,
    linkToObsTab:      Boolean = false,
    onCtrlClick:       Observation.Id => Callback = _ => Callback.empty,
    ctx:               AppContext[IO]
  )(
    obs:               Observation,
    idx:               Int
  ): TagMod =
    <.div(ExploreStyles.ObsTreeItem)(
      Draggable(obs.id.toString, idx, isDragDisabled = readonly) { case (provided, snapshot, _) =>
        <.div(
          provided.innerRef,
          provided.draggableProps,
          getDraggedStyle(provided.draggableStyle, snapshot),
          (^.onClick ==> { (e: ReactMouseEvent) =>
            e.preventDefaultCB *> e.stopPropagationCB >>
              (if (e.ctrlKey || e.metaKey)
                 onCtrlClick(obs.id)
               else onSelect(obs.id))
          }).when(selectable),
          (^.onDoubleClick ==> { (e: ReactEvent) =>
            e.preventDefaultCB *> e.stopPropagationCB >> setObs(programId, obs.id.some, ctx)
          }).when(linkToObsTab)
        )(
          <.span(provided.dragHandleProps)(
            renderObsBadge(obs, layout, highlightSelected, forceHighlight, onDelete)
          )
        )
      }
    )

  // Adapted from https://github.com/atlassian/react-beautiful-dnd/issues/374#issuecomment-569817782
  def getDraggedStyle(style: TagMod, snapshot: Draggable.StateSnapshot): TagMod =
    if (!snapshot.isDragging)
      TagMod.empty
    else if (!snapshot.isDropAnimating)
      style
    else
      TagMod(style, ^.transitionDuration := "0.001s")

  def getListStyle(isDragging: Boolean): TagMod =
    ExploreStyles.DraggingOver.when(isDragging)

  def undoableDeleteObs(
    obsId:        Observation.Id,
    observations: UndoSetter[ObservationList],
    afterUndo:    Observation.Id => Callback,
    afterDelete:  Callback
  )(using
    FetchClient[IO, ObservationDB],
    Logger[IO],
    ToastCtx[IO]
  ): Callback =
    ObsActions
      .obsExistence(
        obsId,
        oid =>
          ToastCtx[IO].showToast(s"Restore deleted obs: ${obsId.show}").runAsyncAndForget *>
            afterUndo(oid)
      )
      .mod(observations)(obsListMod.delete)
      .flatMap(_ => afterDelete)
      .showToastCB(s"Deleted obs: ${obsId.show}")

}
