// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Order._
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
import explore.model.TargetEnvGroup
import explore.model.TargetEnvGroupId
import explore.model.TargetEnvGroupIdSet
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
import react.semanticui.elements.segment.SegmentGroup
import react.semanticui.sizes._

import scala.collection.immutable.SortedSet

final case class TargetListGroupObsList(
  targetListsWithObs: View[TargetListGroupWithObs],
  focusedObs:         View[Option[FocusedObs]],
  selected:           View[SelectedPanel[TargetEnvGroupIdSet]],
  expandedIds:        View[SortedSet[TargetEnvGroupIdSet]],
  undoStacks:         View[UndoStacks[IO, TargetListGroupList]]
)(implicit val ctx:   AppContextIO)
    extends ReactProps[TargetListGroupObsList](TargetListGroupObsList.component)
    with ViewCommon

object TargetListGroupObsList {
  type Props = TargetListGroupObsList

  case class State(dragging: Boolean = false)

  object State {
    val dragging: Lens[State, Boolean] = Focus[State](_.dragging)
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive
  implicit val stateReuse: Reusability[State] = Reusability.derive

  class Backend($ : BackendScope[Props, State]) {

    def toggleExpanded(
      targetEnvGroupIds: TargetEnvGroupIdSet,
      expandedIds:       View[SortedSet[TargetEnvGroupIdSet]]
    ): SyncIO[Unit] =
      expandedIds.mod { expanded =>
        expanded
          .exists(_ === targetEnvGroupIds)
          .fold(expanded - targetEnvGroupIds, expanded + targetEnvGroupIds)
      }

    def parseDragId(dragId: String): Option[Either[TargetEnvironment.Id, Observation.Id]] =
      Observation.Id
        .parse(dragId)
        .map(_.asRight)
        .orElse(TargetEnvironment.Id.parse(dragId).map(_.asLeft))

    def dragIdToTargetEnvGroupId(dragId: String, observations: ObsList): Option[TargetEnvGroupId] =
      parseDragId(dragId: String).flatMap {
        case Left(teid)   => TargetEnvGroupId((teid, none)).some
        case Right(obsId) =>
          observations.get(obsId).map(summ => TargetEnvGroupId((summ.targetEnvId, obsId.some)))
      }

    /**
     * When we're dragging, we can have either an observation id or target env id as the draggable
     * id. If we have a selection, and that id is part of the selection, we drag all the items in
     * the selection. However, the user may have something selected, but be dragging something that
     * is NOT in the selection - in which case we just drag the individual item.
     */
    def getDraggedIds(dragId: String, props: Props): Option[TargetEnvGroupIdSet] =
      dragIdToTargetEnvGroupId(dragId, props.targetListsWithObs.get.observations).map { dId =>
        val dIdSet = TargetEnvGroupIdSet.one(dId)
        props.selected.get.optValue.fold(dIdSet) { selectedIds =>
          if (selectedIds.contains(dId)) selectedIds
          else dIdSet
        }
      }

    def onDragEnd(
      undoCtx:     UndoCtx[TargetListGroupList],
      expandedIds: View[SortedSet[TargetEnvGroupIdSet]],
      selected:    View[SelectedPanel[TargetEnvGroupIdSet]]
    )(implicit
      c:           TransactionalClient[IO, ObservationDB]
    ): (DropResult, ResponderProvided) => SyncIO[Unit] = (result, _) =>
      $.propsIn[SyncIO].flatMap { props =>
        val oData = for {
          destination <- result.destination.toOption
          destIds     <- TargetEnvGroupIdSet.format.getOption(destination.droppableId)
          draggedIds  <- getDraggedIds(result.draggableId, props)
          if destIds.intersect(draggedIds).isEmpty
          destTlg     <-
            props.targetListsWithObs.get.targetListGroups.values
              .find(_.id === destIds)
        } yield (destTlg, draggedIds)

        oData.fold(SyncIO.unit) { case (destTlg, draggedIds) =>
          val targetIds = props.targetListsWithObs.get.targetIdsFor(draggedIds)
          TargetListGroupObsListActions
            .obsTargetListGroup[IO](draggedIds, targetIds, expandedIds, selected)
            .set(undoCtx)(destTlg.some)
        }
      }

    def render(props: Props) = {
      implicit val ctx = props.ctx

      val observations     = props.targetListsWithObs.get.observations
      val targetListGroups = props.targetListsWithObs.get.targetListGroups.map(_._2)

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
          getDraggedIds(rubric.draggableId, props)
            .flatMap(ids =>
              ids.firstAndOnlyObsId.fold {
                val list        = ids.toList.sortBy(id => (id.optObsId, id.targetEnvId))
                val div: TagMod = <.div(
                  SegmentGroup(
                    list.toTagMod(id =>
                      Segment(id.optObsId.map(_.show).getOrElse(id.targetEnvId.show))
                    )
                  )
                )
                div.some
              }(obsId => observations.get(obsId).map(summ => props.renderObsBadge(summ)))
            )
            .getOrElse(<.span("ERROR"))
        )

      val handleDragEnd = onDragEnd(undoCtx, props.expandedIds, props.selected)

      def isTargetEnvSelected(targetEnvId: TargetEnvironment.Id): Boolean =
        props.selected.get.optValue.exists(_.exists(_.targetEnvId === targetEnvId))

      def setSelectedPanelToSet(targetEnvGroupIdSet: TargetEnvGroupIdSet): SyncIO[Unit] =
        props.selected.set(SelectedPanel.editor(targetEnvGroupIdSet))

      def setSelectedPanelToSingle(targetEnvGroupId: TargetEnvGroupId): SyncIO[Unit] =
        setSelectedPanelToSet(TargetEnvGroupIdSet.one(targetEnvGroupId))

      def setSelectedPanelAndObs(targetEnvGroupId: TargetEnvGroupId): SyncIO[Unit] =
        props.focusedObs.set(targetEnvGroupId.optObsId.map(FocusedObs(_))) >>
          setSelectedPanelToSingle(targetEnvGroupId)

      def setSelectedPanelAndObsToSet(targetEnvGroupIdSet: TargetEnvGroupIdSet): SyncIO[Unit] = {
        val focused = targetEnvGroupIdSet.firstAndOnlyObsId.map(FocusedObs(_))
        props.focusedObs.set(focused) >> setSelectedPanelToSet(targetEnvGroupIdSet)
      }

      def clearSelectedPanelAndObs: SyncIO[Unit] =
        props.focusedObs.set(None) >> props.selected.set(SelectedPanel.tree)

      def handleCtrlClick(targetEnvGroupId: TargetEnvGroupId, groupIds: TargetEnvGroupIdSet) =
        props.selected.get.optValue.fold(setSelectedPanelAndObs(targetEnvGroupId)) { selectedIds =>
          if (selectedIds.forall(sid => groupIds.contains(sid))) {
            if (selectedIds.contains(targetEnvGroupId)) {
              selectedIds.removeOne(targetEnvGroupId).fold(clearSelectedPanelAndObs) {
                setSelectedPanelAndObsToSet
              }
            } else
              setSelectedPanelAndObsToSet(selectedIds.add(targetEnvGroupId))
          } else SyncIO.unit // Not in the same group
        }

      def renderGroup(targetListGroup: TargetEnvGroup): VdomNode = {
        val obsIds            = targetListGroup.obsIds
        val targetEnvGroupIds = targetListGroup.id
        val cgObs             = obsIds.toList.map(id => observations.get(id)).flatten
        // if this group or something in it is selected
        val groupSelected     =
          props.selected.get.optValue.exists(_.intersects(targetEnvGroupIds))

        val unmooredTargetEnvIds =
          targetListGroup.id.unmooredTargetEnvIds.toList.sorted

        val icon: FontAwesomeIcon = props.expandedIds.get
          .exists(_ === targetEnvGroupIds)
          .fold(Icons.ChevronDown, Icons.ChevronRight)
          .addModifiers(
            Seq(
              ^.cursor.pointer,
              ^.onClick ==> { e: ReactEvent =>
                e.stopPropagationCB >>
                  toggleExpanded(targetEnvGroupIds, props.expandedIds).toCB
                    .asEventDefault(e)
                    .void
              }
            )
          )
          .fixedWidth()

        Droppable(TargetEnvGroupIdSet.format.reverseGet(targetEnvGroupIds),
                  renderClone = renderClone
        ) { case (provided, snapshot) =>
          val csHeader = <.span(ExploreStyles.ObsTreeGroupHeader)(
            icon,
            <.span(ExploreStyles.ObsGroupTitleWithWrap)(
              targetListGroup.name.value
            ),
            Icons.Thumbtack.when(unmooredTargetEnvIds.nonEmpty),
            <.span(ExploreStyles.ObsCount, s"${obsIds.size} Obs")
          )

          <.div(
            provided.innerRef,
            provided.droppableProps,
            props.getListStyle(
              snapshot.draggingOverWith.exists(id => parseDragId(id).isDefined)
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
                  props.selected.set(SelectedPanel.editor(targetEnvGroupIds))
              }
            )(
              csHeader,
              TagMod.when(props.expandedIds.get.contains(targetEnvGroupIds))(
                TagMod(
                  unmooredTargetEnvIds.zipWithIndex.toTagMod { case (tenvId, idx) =>
                    props.renderTargetEnvBadgeItem(isTargetEnvSelected(tenvId))(tenvId, idx)
                  },
                  cgObs.zipWithIndex.toTagMod { case (obs, idx) =>
                    props.renderObsBadgeItem(
                      selectable = true,
                      highlightSelected = true,
                      forceHighlight = isTargetEnvSelected(obs.targetEnvId),
                      linkToObsTab = false,
                      onSelect = _ =>
                        setSelectedPanelToSingle(
                          TargetEnvGroupId((obs.targetEnvId, obs.id.some))
                        ),
                      onCtrlClick = _ =>
                        handleCtrlClick(TargetEnvGroupId((obs.targetEnvId, obs.id.some)),
                                        targetEnvGroupIds
                        )
                    )(obs, idx)
                  }
                )
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
            Button(onClick =
                     props.focusedObs.set(none) >> props.selected.set(SelectedPanel.summary),
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
      val targetListsWithObs  = $.props.targetListsWithObs.get
      val targetListGroups    = targetListsWithObs.targetListGroups
      val observations        = targetListsWithObs.observations
      val expandedIds         = $.props.expandedIds
      val selected            = $.props.selected
      val targetEnvGroupIdMap = targetListGroups.values.map(tlg => (tlg.id, tlg)).toMap

      // Unfocus if focused element is not there
      val unfocus = $.props.focusedObs.mod(_.flatMap {
        case FocusedObs(obsId) if !observations.contains(obsId) => none
        case other                                              => other.some
      })

      val setAndGetSelected: SyncIO[Option[TargetEnvGroup]] = selected.get match {
        case Uninitialized =>
          val infoFromFocused: Option[(TargetEnvGroupId, TargetEnvGroup)] =
            $.props.focusedObs.get.flatMap(fo =>
              (observations
                 .get(fo.obsId)
                 .map(summ => TargetEnvGroupId((summ.targetEnvId, summ.id.some))),
               targetListGroups.find(_._1.exists(_.optObsId === fo.obsId.some)).map(_._2)
              ).tupled
            )

          selected
            .set(infoFromFocused.fold(SelectedPanel.tree[TargetEnvGroupIdSet]) { case (id, _) =>
              SelectedPanel.editor(TargetEnvGroupIdSet.one(id))
            })
            .as(infoFromFocused.map(_._2))
        case Editor(ids)   =>
          SyncIO.delay(targetEnvGroupIdMap.find(_._1.intersect(ids).nonEmpty).map(_._2))
        case _             => SyncIO.delay(none)
      }

      def expandSelected(tlgOpt: Option[TargetEnvGroup]) =
        tlgOpt.map(tlg => expandedIds.mod(_ + tlg.id)).orEmpty

      def cleanupExpandedIds =
        expandedIds.mod(_.filter(targetEnvGroupIdMap.contains))

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
