// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import explore._
import explore.components.ui.ExploreStyles
import explore.model.Focused
import explore.model.Focused._
import explore.model.ObsSummary
import japgolly.scalajs.react.ReactEvent
import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.vdom.html_<^._
import org.typelevel.log4cats.Logger
import react.beautifuldnd._

trait ViewCommon {
  def focused: View[Option[Focused]]
  def obsBadgeLayout: ObsBadge.Layout

  def renderObsBadge(obs: ObsSummary): TagMod =
    ObsBadge(
      obs,
      obsBadgeLayout,
      selected = focused.get.exists(_ === FocusedObs(obs.id))
    )

  def renderObsBadgeItem(obs: ObsSummary, idx: Int)(implicit logger: Logger[IO]): TagMod =
    <.div(ExploreStyles.ObsTreeItem)(
      Draggable(obs.id.toString, idx) { case (provided, snapshot, _) =>
        <.div(
          provided.innerRef,
          provided.draggableProps,
          getDraggedStyle(provided.draggableStyle, snapshot),
          ^.onClick ==> { e: ReactEvent =>
            e.stopPropagationCB >> focused.set(FocusedObs(obs.id).some).runAsyncCB
          }
        )(<.span(provided.dragHandleProps)(renderObsBadge(obs)))
      }
    )

  // Adapted from https://github.com/atlassian/react-beautiful-dnd/issues/374#issuecomment-569817782
  def getDraggedStyle(style:   TagMod, snapshot: Draggable.StateSnapshot): TagMod =
    if (!snapshot.isDragging)
      TagMod.empty
    else if (!snapshot.isDropAnimating)
      style
    else
      TagMod(style, ^.transitionDuration := "0.001s")

  def getListStyle(isDragging: Boolean): TagMod =
    ExploreStyles.DraggingOver.when(isDragging)
}
