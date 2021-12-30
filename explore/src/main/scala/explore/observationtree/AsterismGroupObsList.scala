// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all._
import clue.TransactionalClient
import crystal.ViewF
import crystal.react.View
import crystal.react.implicits._
import explore.Icons
import explore.common.AsterismQueries._
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.implicits._
import explore.model.AsterismGroup
import explore.model.FocusedObs
import explore.model.ObsIdSet
import explore.model.SelectedPanel
import explore.model.SelectedPanel._
import explore.model.TargetWithId
import explore.model.reusability._
import explore.undo.UndoContext
import explore.undo._
import japgolly.scalajs.react._
import japgolly.scalajs.react.callback.CallbackCats._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
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

final case class AsterismGroupObsList(
  asterismsWithObs: View[AsterismGroupsWithObs],
  focusedObs:       View[Option[FocusedObs]],
  selected:         View[SelectedPanel[ObsIdSet]],
  expandedIds:      View[SortedSet[ObsIdSet]],
  undoStacks:       View[UndoStacks[IO, AsterismGroupList]]
)(implicit val ctx: AppContextIO)
    extends ReactProps[AsterismGroupObsList](AsterismGroupObsList.component)
    with ViewCommon

object AsterismGroupObsList {
  type Props = AsterismGroupObsList

  case class State(dragging: Boolean = false)

  object State {
    val dragging: Lens[State, Boolean] = Focus[State](_.dragging)
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive
  implicit val stateReuse: Reusability[State] = Reusability.derive

  class Backend($ : BackendScope[Props, State]) {

    def toggleExpanded(
      obsIds:      ObsIdSet,
      expandedIds: View[SortedSet[ObsIdSet]]
    ): Callback =
      expandedIds.mod { expanded =>
        expanded
          .exists(_ === obsIds)
          .fold(expanded - obsIds, expanded + obsIds)
      }

    // TODO: We will need this when we start being able to drag targets.
    // def parseDragId(dragId: String): Option[Either[Target.Id, Observation.Id]] =
    //   Observation.Id
    //     .parse(dragId)
    //     .map(_.asRight)
    //     .orElse(Target.Id.parse(dragId).map(_.asLeft))

    def parseDragId(dragId: String): Option[Observation.Id] = Observation.Id.parse(dragId)

    /**
     * When we're dragging, we can either be dragging a single observation or a group of them. If we
     * have a selection, and the drag id is part of the selection, we drag all the items in the
     * selection. However, the user may have something selected, but be dragging something that is
     * NOT in the selection - in which case we just drag the individual item.
     */
    def getDraggedIds(dragId: String, props: Props): Option[ObsIdSet] =
      parseDragId(dragId).map { obsId =>
        props.selected.get.optValue.fold(ObsIdSet.one(obsId)) { selectedIds =>
          if (selectedIds.contains(obsId)) selectedIds
          else ObsIdSet.one(obsId)
        }
      }

    def onDragEnd(
      undoCtx:     UndoContext[AsterismGroupList],
      expandedIds: View[SortedSet[ObsIdSet]],
      focusedObs:  View[Option[FocusedObs]],
      selected:    View[SelectedPanel[ObsIdSet]]
    )(implicit
      c:           TransactionalClient[IO, ObservationDB]
    ): (DropResult, ResponderProvided) => Callback = (result, _) =>
      $.props.flatMap { props =>
        val oData = for {
          destination <- result.destination.toOption
          destIds     <- ObsIdSet.fromString.getOption(destination.droppableId)
          draggedIds  <- getDraggedIds(result.draggableId, props)
          if destIds.intersect(draggedIds).isEmpty
          destAg      <-
            props.asterismsWithObs.get.asterismGroups.values
              .find(_.obsIds === destIds)
        } yield (destAg, draggedIds)

        oData.foldMap { case (destAg, draggedIds) =>
          AsterismGroupObsListActions
            .obsAsterismGroup(draggedIds, expandedIds, selected, focusedObs)
            .set(undoCtx)(destAg.some)
        }
      }

    def render(props: Props) = {
      implicit val ctx = props.ctx

      val observations   = props.asterismsWithObs.get.observations
      val asterismGroups = props.asterismsWithObs.get.asterismGroups.map(_._2)
      val targetMap      = props.asterismsWithObs.get.targetGroups

      val state   = ViewF.fromState($)
      val undoCtx = UndoContext(
        props.undoStacks,
        props.asterismsWithObs.zoom(AsterismGroupsWithObs.asterismGroups)
      )

      val renderClone: Draggable.Render = (provided, snapshot, rubric) =>
        <.div(provided.innerRef,
              provided.draggableProps,
              provided.dragHandleProps,
              props.getDraggedStyle(provided.draggableStyle, snapshot)
        )(
          getDraggedIds(rubric.draggableId, props)
            .flatMap(ids =>
              ids.firstAndOnly.fold {
                val list        = ids.toList
                val div: TagMod = <.div(
                  SegmentGroup(
                    list.toTagMod(id => Segment(id.show))
                  )
                )
                div.some
              }(obsId => observations.get(obsId).map(summ => props.renderObsBadge(summ)))
            )
            .getOrElse(<.span("ERROR"))
        )

      val handleDragEnd = onDragEnd(undoCtx, props.expandedIds, props.focusedObs, props.selected)

      def isObsSelected(obsId: Observation.Id): Boolean =
        props.selected.get.optValue.exists(_.exists(_ === obsId))

      def setSelectedPanelToSet(obsIdSet: ObsIdSet): Callback =
        props.selected.set(SelectedPanel.editor(obsIdSet))

      def setSelectedPanelToSingle(obsId: Observation.Id): Callback =
        setSelectedPanelToSet(ObsIdSet.one(obsId))

      def setSelectedPanelAndObs(obsId: Observation.Id): Callback =
        props.focusedObs.set(FocusedObs(obsId).some) >>
          setSelectedPanelToSingle(obsId)

      def setSelectedPanelAndObsToSet(obsIdSet: ObsIdSet): Callback = {
        val focused = obsIdSet.firstAndOnly.map(FocusedObs(_))
        props.focusedObs.set(focused) >> setSelectedPanelToSet(obsIdSet)
      }

      def clearSelectedPanelAndObs: Callback =
        props.focusedObs.set(None) >> props.selected.set(SelectedPanel.tree)

      def handleCtrlClick(obsIds: Observation.Id, groupIds: ObsIdSet) =
        props.selected.get.optValue.fold(setSelectedPanelAndObs(obsIds)) { selectedIds =>
          if (selectedIds.subsetOf(groupIds)) {
            if (selectedIds.contains(obsIds)) {
              selectedIds.removeOne(obsIds).fold(clearSelectedPanelAndObs) {
                setSelectedPanelAndObsToSet
              }
            } else
              setSelectedPanelAndObsToSet(selectedIds.add(obsIds))
          } else Callback.empty // Not in the same group
        }

      def getAsterismGroupName(asterismGroup: AsterismGroup): String = {
        val targets = asterismGroup.targetIds.toList.map(targetMap.get).flatten
        if (targets.isEmpty) "<No Targets>"
        else targets.map(TargetWithId.name.get).mkString(";")
      }

      def renderGroup(asterismGroup: AsterismGroup): VdomNode = {
        val obsIds        = asterismGroup.obsIds
        val cgObs         = obsIds.toList.map(id => observations.get(id)).flatten
        // if this group or something in it is selected
        val groupSelected =
          props.selected.get.optValue.exists(_.intersects(obsIds))

        val icon: FontAwesomeIcon = props.expandedIds.get
          .exists(_ === obsIds)
          .fold(Icons.ChevronDown, Icons.ChevronRight)
          .addModifiers(
            Seq(
              ^.cursor.pointer,
              ^.onClick ==> { e: ReactEvent =>
                e.stopPropagationCB >>
                  toggleExpanded(obsIds, props.expandedIds)
                    .asEventDefault(e)
                    .void
              }
            )
          )
          .fixedWidth()

        Droppable(ObsIdSet.fromString.reverseGet(obsIds), renderClone = renderClone) {
          case (provided, snapshot) =>
            val csHeader = <.span(ExploreStyles.ObsTreeGroupHeader)(
              icon,
              <.span(ExploreStyles.ObsGroupTitleWithWrap)(
                getAsterismGroupName(asterismGroup)
              ),
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
              )(
                ^.cursor.pointer,
                ^.onClick --> {
                  setSelectedPanelAndObsToSet(obsIds)
                }
              )(
                csHeader,
                TagMod.when(props.expandedIds.get.contains(obsIds))(
                  TagMod(
                    cgObs.zipWithIndex.toTagMod { case (obs, idx) =>
                      props.renderObsBadgeItem(
                        selectable = true,
                        highlightSelected = true,
                        forceHighlight = isObsSelected(obs.id),
                        linkToObsTab = false,
                        onSelect = _ => setSelectedPanelToSingle(obs.id),
                        onCtrlClick = _ => handleCtrlClick(obs.id, obsIds)
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
        onDragStart = (_: DragStart, _: ResponderProvided) => state.zoom(State.dragging).set(true),
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
              asterismGroups.toTagMod(renderGroup)
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
      val asterismsWithObs = $.props.asterismsWithObs.get
      val asterismGroups   = asterismsWithObs.asterismGroups
      val observations     = asterismsWithObs.observations
      val expandedIds      = $.props.expandedIds
      val selected         = $.props.selected

      // Unfocus if focused element is not there
      val unfocus = $.props.focusedObs.mod(_.flatMap {
        case FocusedObs(obsId) if !observations.contains(obsId) => none
        case other                                              => other.some
      })

      val setAndGetSelected: CallbackTo[Option[AsterismGroup]] = selected.get match {
        case Uninitialized =>
          val infoFromFocused: Option[(Observation.Id, AsterismGroup)] =
            $.props.focusedObs.get.flatMap(fo =>
              (fo.obsId.some, asterismGroups.find(_._1.exists(_ === fo.obsId)).map(_._2)).tupled
            )

          selected
            .set(infoFromFocused.fold(SelectedPanel.tree[ObsIdSet]) { case (id, _) =>
              SelectedPanel.editor(ObsIdSet.one(id))
            })
            .as(infoFromFocused.map(_._2))
        case Editor(ids)   =>
          CallbackTo(asterismGroups.find(_._1.intersect(ids).nonEmpty).map(_._2))
        case _             => CallbackTo(none)
      }

      def expandSelected(agOpt: Option[AsterismGroup]) =
        agOpt.map(ag => expandedIds.mod(_ + ag.obsIds)).orEmpty

      def cleanupExpandedIds =
        expandedIds.mod(_.filter(asterismGroups.contains))

      for {
        _     <- unfocus
        agOpt <- setAndGetSelected
        _     <- expandSelected(agOpt)
        _     <- cleanupExpandedIds
      } yield ()
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
