// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.syntax.all._
import explore._
import explore.components.ui.ExploreStyles
import explore.model.FocusedObs
import explore.model.ObsSummary
import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.ReactEvent
import japgolly.scalajs.react.ReactMouseEvent
import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.model.TargetEnvironment
import react.beautifuldnd._
import react.semanticui.views.card._

trait ViewCommon {
  def focusedObs: View[Option[FocusedObs]]

  def renderObsBadge(
    obs:               ObsSummary,
    highlightSelected: Boolean = true,
    forceHighlight:    Boolean = false // if true, overrides highlightSelected
  ): TagMod =
    ObsBadge(
      obs,
      selected =
        forceHighlight || (highlightSelected && focusedObs.get.exists(_ === FocusedObs(obs.id)))
    )

  def renderObsBadgeItem(
    selectable:        Boolean,
    highlightSelected: Boolean = true,
    forceHighlight:    Boolean = false,
    linkToObsTab:      Boolean = false,
    onSelect:          Observation.Id => Callback = _ => Callback.empty,
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
               else
                 (focusedObs.set(FocusedObs(obs.id).some) >> onSelect(obs.id)))
          }).when(selectable),
          (^.onDoubleClick ==> { e: ReactEvent =>
            e.stopPropagationCB >>
              ctx.setPage(explore.model.enum.AppTab.Observations, FocusedObs(obs.id).some)
          }).when(linkToObsTab)
        )(<.span(provided.dragHandleProps)(renderObsBadge(obs, highlightSelected, forceHighlight)))
      }
    )

  def renderTargetEnvBadge(
    targetEnvId: TargetEnvironment.Id,
    selected:    Boolean
  ): TagMod =
    <.div(
      Card(raised = selected)(ExploreStyles.ObsBadge)(
        CardContent(
          CardHeader(<.span(ExploreStyles.ObsBadgeHeader, targetEnvId.toString))
        )
      )
    )

  def renderTargetEnvBadgeItem(
    selected:    Boolean
    // onSelect:    TargetEnvironment.Id => Callback = _ => Callback.empty,
    // onCtrlClick: TargetEnvironment.Id => Callback = _ => Callback.empty
  )(targetEnvId: TargetEnvironment.Id, idx: Int): TagMod =
    <.div(ExploreStyles.ObsTreeItem)(Draggable(targetEnvId.toString, idx) {
      case (provided, snapshot, _) =>
        <.div(
          provided.innerRef,
          provided.draggableProps,
          getDraggedStyle(provided.draggableStyle, snapshot),
          // TODO: Currently, clicking will select the whole group. When the API
          // is updated to allow us to edit individual unmoored target environments,
          // the onclick handler will be used.
          // ^.onClick ==> { e: ReactMouseEvent =>
          //   e.stopPropagationCB >>
          //     (if (e.ctrlKey || e.metaKey) onCtrlClick(targetEnvId)
          //      else onSelect(targetEnvId))
          // },
          (<.span(provided.dragHandleProps)(renderTargetEnvBadge(targetEnvId, selected)))
        )
    })

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
