// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Order._
import cats.data.NonEmptySet
import cats.effect.IO
import cats.effect.SyncIO
import cats.syntax.all._
import clue.TransactionalClient
import crystal.ViewF
import crystal.react.implicits._
import explore.Icons
import explore.common.TargetListGroupQueries._
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.implicits._
import explore.model.FocusedObs
import explore.model.SelectedPanel
import explore.model.SelectedPanel._
import explore.model.TargetEnv
import explore.model.TargetEnvIdObsId
import explore.model.TargetEnvIdObsIdSet
import explore.model.reusability._
import explore.undo._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.model.TargetEnvironment
import lucuma.schemas.ObservationDB
import lucuma.ui.reusability._
import monocle.Focus
import monocle.Lens
import mouse.boolean._
import react.beautifuldnd._
import react.common._
import react.common.implicits._
import react.fa.FontAwesomeIcon
import react.semanticui.elements.button.Button
import react.semanticui.elements.segment.Segment
import react.semanticui.sizes._

import scala.collection.immutable.SortedSet

final case class TargetListGroupObsList(
  targetListsWithObs: View[TargetListGroupWithObs],
  focusedObs:         View[Option[FocusedObs]],
  selected:           View[SelectedPanel[TargetEnvIdObsIdSet]],
  expandedIds:        View[SortedSet[TargetEnvIdObsIdSet]],
  undoStacks:         View[UndoStacks[IO, TargetListGroupList]]
)(implicit val ctx:   AppContextIO)
    extends ReactProps[TargetListGroupObsList](TargetListGroupObsList.component)
    with ViewCommon {
  lazy val envIdToObsMap: Map[TargetEnvironment.Id, Observation.Id] =
    targetListsWithObs.get.observations.map { case (id, obsSumm) => (obsSumm.targetEnvId, id) }
}

object TargetListGroupObsList {
  type Props = TargetListGroupObsList

  case class State(dragging: Boolean = false)

  object State {
    val dragging: Lens[State, Boolean] = Focus[State](_.dragging)
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive
  implicit val stateReuse: Reusability[State] = Reusability.derive

  class Backend($ : BackendScope[Props, State]) {
    def targetEnvIdObsIdToString(ids: TargetEnvIdObsIdSet): String =
      ids.toSortedSet.map(_._1).mkString(",") // just keep the target ids to make parsing easier

    def targetEnvIdObsIdStringToIds(
      targetEnvObsIdsString: String,
      obsMap:                Map[TargetEnvironment.Id, Observation.Id]
    ): Option[TargetEnvIdObsIdSet] =
      targetEnvObsIdsString
        .split(",")
        .toList
        .traverse(TargetEnvironment.Id.parse(_))
        .map(_.map(teId => (teId, obsMap.get(teId))))
        .flatMap(list => NonEmptySet.fromSet(SortedSet.from(list)))

    def toggleExpanded(
      targetEnvIds: TargetEnvIdObsIdSet,
      expandedIds:  View[SortedSet[TargetEnvIdObsIdSet]]
    ): SyncIO[Unit] =
      expandedIds.mod { expanded =>
        expanded.exists(_ === targetEnvIds).fold(expanded - targetEnvIds, expanded + targetEnvIds)
      }

    def onDragEnd(
      undoCtx:     UndoCtx[TargetListGroupList],
      expandedIds: View[SortedSet[TargetEnvIdObsIdSet]],
      selected:    View[SelectedPanel[TargetEnvIdObsIdSet]]
    )(implicit
      c:           TransactionalClient[IO, ObservationDB]
    ): (DropResult, ResponderProvided) => SyncIO[Unit] = (result, _) =>
      $.propsIn[SyncIO].flatMap { props =>
        val oData = for {
          destination <- result.destination.toOption
          destIds     <- targetEnvIdObsIdStringToIds(destination.droppableId, props.envIdToObsMap)
          obsId       <- Observation.Id.parse(result.draggableId)
          targetEnvId <- props.targetListsWithObs.get.observations.get(obsId).map(_.targetEnvId)
          if !destIds.contains((targetEnvId, obsId.some))
          destTlg     <-
            props.targetListsWithObs.get.targetListGroups.values
              .find(_.id === destIds)
        } yield (destTlg, obsId, targetEnvId)

        oData.fold(SyncIO.unit) { case (destTlg, obsId, targetEnvId) =>
          TargetListGroupObsListActions
            .obsTargetListGroup[IO](obsId, targetEnvId, expandedIds, selected)
            .set(undoCtx)(destTlg.scienceTargets.some)
        }
      }

    def render(props: Props) = {
      implicit val ctx = props.ctx

      val observations     = props.targetListsWithObs.get.observations
      val targetListGroups = props.targetListsWithObs.get.targetListGroups.map(_._2)

      // if a single observation is selected
      val singleObsSelected = props.focusedObs.get.nonEmpty

      val state   = ViewF.fromStateSyncIO($)
      val undoCtx = UndoContext(
        props.undoStacks,
        props.targetListsWithObs.zoom(TargetListGroupWithObs.targetListGroups)
      )

      val renderClone: Draggable.Render = (provided, snapshot, rubric) =>
        <.div(provided.innerRef,
              provided.draggableProps,
              provided.dragHandleProps,
              props.getDraggedStyle(provided.draggableStyle, snapshot)
        )(
          Observation.Id
            .parse(rubric.draggableId)
            .flatMap(obsId => observations.get(obsId))
            .map(obs => props.renderObsBadge(obs))
            .getOrElse(<.span("ERROR"))
        )

      val handleDragEnd = onDragEnd(undoCtx, props.expandedIds, props.selected)

      def renderGroup(targetListGroup: TargetEnv): VdomNode = {
        val obsIds          = targetListGroup.obsIds
        val targetEnvObsIds = targetListGroup.id
        val cgObs           = obsIds.toList.map(id => observations.get(id)).flatten
        // if this group or something in it is selected
        val groupSelected   =
          props.selected.get.optValue.exists(_.intersect(targetEnvObsIds).nonEmpty)

        val icon: FontAwesomeIcon = props.expandedIds.get
          .exists(_ === targetEnvObsIds)
          .fold(Icons.ChevronDown, Icons.ChevronRight)
          .addModifiers(
            Seq(
              ^.cursor.pointer,
              ^.onClick ==> { e: ReactEvent =>
                e.stopPropagationCB >>
                  toggleExpanded(targetEnvObsIds, props.expandedIds).toCB
                    .asEventDefault(e)
                    .void
              }
            )
          )
          .fixedWidth()

        Droppable(targetEnvIdObsIdToString(targetEnvObsIds), renderClone = renderClone) {
          case (provided, snapshot) =>
            val csHeader = <.span(ExploreStyles.ObsTreeGroupHeader)(
              icon,
              <.span(ExploreStyles.ObsGroupTitleWithWrap)(
                targetListGroup.name
              ),
              Icons.Thumbtack.when(obsIds.size < targetEnvObsIds.size),
              <.span(ExploreStyles.ObsCount, s"${obsIds.size} Obs")
            )

            <.div(
              provided.innerRef,
              provided.droppableProps,
              props.getListStyle(
                snapshot.draggingOverWith.exists(id => Observation.Id.parse(id).isDefined)
              )
            )(
              Segment(
                vertical = true,
                clazz = ExploreStyles.ObsTreeGroup |+| Option
                  .when(groupSelected)(ExploreStyles.SelectedObsTreeGroup)
                  .orElse(
                    Option.when(!state.get.dragging)(ExploreStyles.UnselectedObsTreeGroup)
                  )
                  .orEmpty
              )(^.cursor.pointer,
                ^.onClick --> {
                  props.focusedObs.set(none) >>
                    props.selected.set(SelectedPanel.editor(targetEnvObsIds))
                }
              )(
                csHeader,
                TagMod.when(props.expandedIds.get.contains(targetEnvObsIds))(
                  cgObs.zipWithIndex.toTagMod { case (obs, idx) =>
                    props.renderObsBadgeItem(
                      selectable = true,
                      highlightSelected = true,
                      forceHighlight = groupSelected && !singleObsSelected,
                      linkToObsTab = false,
                      onSelect = _ =>
                        props.selected.set(
                          SelectedPanel.editor(NonEmptySet.one((obs.targetEnvId, obs.id.some)))
                        )
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
          (_: DragStart, _: ResponderProvided) => state.zoom(State.dragging).set(true).toCB,
        onDragEnd = (result, provided) =>
          state.zoom(State.dragging).set(false) >> handleDragEnd(result, provided)
      )(
        <.div(ExploreStyles.ObsTreeWrapper)(
          <.div(ExploreStyles.TreeToolbar)(UndoButtons(undoCtx, size = Mini)),
          <.div(
            Button(onClick = props.selected.set(SelectedPanel.summary),
                   clazz = ExploreStyles.ButtonSummary
            )(
              Icons.ListIcon.clazz(ExploreStyles.PaddedRightIcon),
              "Target List Summary"
            )
          ),
          <.div(ExploreStyles.ObsTree)(
            <.div(ExploreStyles.ObsScrollTree)(
              targetListGroups.toTagMod(renderGroup)
            )
          )
        )
      )
    }
  }

  protected val component = ScalaComponent
    .builder[Props]
    .initialState(State())
    .renderBackend[Backend]
    .componentDidMount { $ =>
      val targetListsWithObs = $.props.targetListsWithObs.get
      val targetListGroups   = targetListsWithObs.targetListGroups
      val observations       = targetListsWithObs.observations
      val expandedIds        = $.props.expandedIds
      val selected           = $.props.selected
      val targetEnvObsIdMap  = targetListGroups.values.map(tlg => (tlg.id, tlg)).toMap

      // Unfocus if focused element is not there
      val unfocus = $.props.focusedObs.mod(_.flatMap {
        case FocusedObs(obsId) if !observations.contains(obsId) => none
        case other                                              => other.some
      })

      val setAndGetSelected: SyncIO[Option[TargetEnv]] = selected.get match {
        case Uninitialized =>
          val infoFromFocused: Option[(TargetEnvIdObsId, TargetEnv)] =
            $.props.focusedObs.get.flatMap(fo =>
              (observations.get(fo.obsId).map(summ => (summ.targetEnvId, summ.id.some)),
               targetListGroups.find(_._1.exists(_._2 === fo.obsId.some)).map(_._2)
              ).tupled
            )

          selected
            .set(infoFromFocused.fold(SelectedPanel.tree[TargetEnvIdObsIdSet]) { case (id, _) =>
              SelectedPanel.editor(NonEmptySet.one(id))
            })
            .as(infoFromFocused.map(_._2))
        case Editor(ids)   =>
          SyncIO.delay(targetEnvObsIdMap.find(_._1.intersect(ids).nonEmpty).map(_._2))
        case _             => SyncIO.delay(none)
      }

      def expandSelected(tlgOpt: Option[TargetEnv]) =
        tlgOpt.map(tlg => expandedIds.mod(_ + tlg.id)).orEmpty

      def cleanupExpandedIds =
        expandedIds.mod(_.filter(targetEnvObsIdMap.contains))

      for {
        _      <- unfocus
        tlgOpt <- setAndGetSelected
        _      <- expandSelected(tlgOpt)
        _      <- cleanupExpandedIds
      } yield ()
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
