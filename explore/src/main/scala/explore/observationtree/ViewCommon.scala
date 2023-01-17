// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.implicits.*
import explore.*
import explore.components.ui.ExploreStyles
import explore.model.AppContext
import explore.model.Focused
import explore.model.ObsIdSet
import explore.model.ObsSummary
import explore.model.enums.AppTab
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.TagMod
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import react.beautifuldnd.*
import explore.undo.UndoContext
import queries.schemas.odb.ObsQueries.*
import explore.model.ObsSummaryWithTitleAndConstraints
import explore.undo.KIListMod
import explore.model.ObsSummaryWithTitleConstraintsAndConf

trait ViewCommon {
  def programId: Program.Id
  def focusedObsSet: Option[ObsIdSet]

  def renderObsBadge(
    obs:               ObsSummary,
    highlightSelected: Boolean = true,
    forceHighlight:    Boolean = false // if true, overrides highlightSelected
  ): TagMod =
    ObsBadge(
      obs,
      selected = forceHighlight || (highlightSelected && focusedObsSet.exists(_.contains(obs.id)))
    )

  def renderObsBadgeItem(
    selectable:        Boolean,
    onSelect:          Observation.Id => Callback,
    highlightSelected: Boolean = true,
    forceHighlight:    Boolean = false,
    linkToObsTab:      Boolean = false,
    onCtrlClick:       Observation.Id => Callback = _ => Callback.empty,
    ctx:               AppContext[IO]
  )(
    obs:               ObsSummary,
    idx:               Int
  ): TagMod =
    <.div(ExploreStyles.ObsTreeItem)(
      Draggable(obs.id.toString, idx) { case (provided, snapshot, _) =>
        <.div(
          provided.innerRef,
          provided.draggableProps,
          getDraggedStyle(provided.draggableStyle, snapshot),
          (^.onClick ==> { (e: ReactMouseEvent) =>
            e.stopPropagationCB >>
              (if (e.ctrlKey || e.metaKey)
                 onCtrlClick(obs.id)
               else onSelect(obs.id))
          }).when(selectable),
          (^.onDoubleClick ==> { (e: ReactEvent) =>
            e.stopPropagationCB >>
              ObsOperations.setObs(programId, obs.id.some, ctx)
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

object ObsOperations:
  val obsListMod =
    KIListMod[ObsSummaryWithTitleConstraintsAndConf, Observation.Id](
      ObsSummaryWithTitleConstraintsAndConf.id
    )

  def setObs[F[_]](
    programId: Program.Id,
    obsId:     Option[Observation.Id],
    ctx:       AppContext[F]
  ): Callback =
    ctx.pushPage(AppTab.Observations, programId, obsId.fold(Focused.None)(Focused.singleObs(_)))

  def cloneObs(
    programId: Program.Id,
    obsId:     Observation.Id,
    pos:       Int,
    undoCtx:   UndoContext[ObservationList],
    setObs:    Observation.Id => Callback,
    aftermod:  ObsSummaryWithTitleAndConstraints => Option[
      ObsListActions.obsListMod.ElemWithIndex
    ] => Option[
      ObsListActions.obsListMod.ElemWithIndex
    ],
    ctx:       AppContext[IO],
    before:    IO[Unit] = IO.unit,
    after:     IO[Unit] = IO.unit
  ): IO[Unit] =
    import ctx.given

    before >>
      cloneObservation[IO](obsId)
        .flatMap { obs =>
          ObsListActions
            .obsExistence(obs.id, setObs)
            .mod(undoCtx)(aftermod(obs))
            .to[IO]
        }
        .guarantee(after)
