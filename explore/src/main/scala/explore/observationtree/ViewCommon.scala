// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.syntax.all._
import explore._
import explore.components.ui.ExploreStyles
import explore.model.ObsSummary
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.ReactEvent
import japgolly.scalajs.react.ReactMouseEvent
import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import react.beautifuldnd._

trait ViewCommon {
  def focusedObs: Option[Observation.Id]

  def renderObsBadge(
    obs:               ObsSummary,
    highlightSelected: Boolean = true,
    forceHighlight:    Boolean = false // if true, overrides highlightSelected
  ): TagMod =
    ObsBadge(
      obs,
      selected = forceHighlight || (highlightSelected && focusedObs.exists(_ === obs.id))
    )

  def renderObsBadgeItem(
    selectable:        Boolean,
    onSelect:          Observation.Id => Callback,
    highlightSelected: Boolean = true,
    forceHighlight:    Boolean = false,
    linkToObsTab:      Boolean = false,
    onCtrlClick:       Observation.Id => Callback = _ => Callback.empty
  )(
    obs:               ObsSummary,
    idx:               Int
  )(implicit ctx:      AppContextIO): TagMod =
    <.div(ExploreStyles.ObsTreeItem)(
      Draggable(obs.id.toString, idx) { case (provided, snapshot, _) =>
        <.div(
          provided.innerRef,
          provided.draggableProps,
          getDraggedStyle(provided.draggableStyle, snapshot),
          (^.onClick ==> { e: ReactMouseEvent =>
            e.stopPropagationCB >>
              (if (e.ctrlKey || e.metaKey)
                 onCtrlClick(obs.id)
               else onSelect(obs.id))
          }).when(selectable),
          (^.onDoubleClick ==> { e: ReactEvent =>
            e.stopPropagationCB >>
              ctx.setPage(explore.model.enum.AppTab.Observations, obs.id.some, None)
          }).when(linkToObsTab)
        )(<.span(provided.dragHandleProps)(renderObsBadge(obs, highlightSelected, forceHighlight)))
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
}
