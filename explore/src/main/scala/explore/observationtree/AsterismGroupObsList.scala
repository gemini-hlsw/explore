// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all._
import clue.TransactionalClient
import crystal.react.ReuseView
import crystal.react.View
import explore.Icons
import explore.common.AsterismQueries._
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.implicits._
import explore.model.AsterismGroup
import explore.model.ObsIdSet
import explore.model.SelectedPanel
import explore.model.SelectedPanel._
import explore.model.TargetGroup
import explore.model.enum.AppTab
import explore.model.reusability._
import explore.undo._
import japgolly.scalajs.react._
import japgolly.scalajs.react.callback.CallbackCats._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.ui.reusability._
import monocle.Focus
import monocle.Lens
import mouse.boolean._
import react.beautifuldnd._
import react.common._
import react.common.implicits._
import react.fa.FontAwesomeIcon
import react.reflex._
import react.semanticui.elements.button.Button
import react.semanticui.elements.header.Header
import react.semanticui.elements.segment.Segment
import react.semanticui.elements.segment.SegmentGroup
import react.semanticui.shorthand._
import react.semanticui.sizes._
import react.semanticui.views.card._

import scala.collection.immutable.SortedSet

final case class AsterismGroupObsList(
  asterismsWithObs: ReuseView[AsterismGroupsWithObs],
  focusedObsSet:    Option[ObsIdSet],
  focusedTarget:    Option[Target.Id],
  selected:         ReuseView[SelectedPanel[AsterismGroupObsList.TargetOrObsSet]],
  expandedIds:      ReuseView[SortedSet[ObsIdSet]],
  undoStacks:       ReuseView[UndoStacks[IO, AsterismGroupsWithObs]]
)(implicit val ctx: AppContextIO)
    extends ReactProps[AsterismGroupObsList](AsterismGroupObsList.component)
    with ViewCommon

object AsterismGroupObsList {
  type Props = AsterismGroupObsList

  // Would be better respresented as a union type in scala 3
  type TargetOrObsSet = Either[Target.Id, ObsIdSet]

  case class State(dragging: Boolean = false)

  object State {
    val dragging: Lens[State, Boolean] = Focus[State](_.dragging)
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive
  implicit val stateReuse: Reusability[State] = Reusability.derive

  class Backend($ : BackendScope[Props, State]) {

    def toggleExpanded(
      obsIds:      ObsIdSet,
      expandedIds: ReuseView[SortedSet[ObsIdSet]]
    ): Callback =
      expandedIds.mod { expanded =>
        expanded
          .exists(_ === obsIds)
          .fold(expanded - obsIds, expanded + obsIds)
      }

    def parseDragId(dragId: String): Option[Either[Target.Id, Observation.Id]] =
      Observation.Id
        .parse(dragId)
        .map(_.asRight)
        .orElse(Target.Id.parse(dragId).map(_.asLeft))

    /**
     * When we're dragging an observation, we can either be dragging a single observation or a group
     * of them. If we have a selection, and the drag id is part of the selection, we drag all the
     * items in the selection. However, the user may have something selected, but be dragging
     * something that is NOT in the selection - in which case we just drag the individual item.
     */
    def getDraggedIds(dragId: String, props: Props): Option[TargetOrObsSet] =
      parseDragId(dragId).map {
        case Left(targetId) => targetId.asLeft
        case Right(obsId)   =>
          props.selected.get.optValue
            .flatMap(_.toOption)
            .fold(ObsIdSet.one(obsId)) { selectedIds =>
              if (selectedIds.contains(obsId)) selectedIds
              else ObsIdSet.one(obsId)
            }
            .asRight
      }

    def onDragEnd(
      undoCtx:     UndoContext[AsterismGroupsWithObs],
      expandedIds: ReuseView[SortedSet[ObsIdSet]],
      selected:    ReuseView[SelectedPanel[TargetOrObsSet]]
    )(implicit
      c:           TransactionalClient[IO, ObservationDB]
    ): (DropResult, ResponderProvided) => Callback = (result, _) =>
      $.props.flatMap { props =>
        val oData = for {
          destination <- result.destination.toOption
          destIds     <- ObsIdSet.fromString.getOption(destination.droppableId)
          draggedIds  <- getDraggedIds(result.draggableId, props)
          destAg      <-
            props.asterismsWithObs.get.asterismGroups.values
              .find(_.obsIds === destIds)
        } yield (destAg, draggedIds)

        def setObsSet(obsIds: ObsIdSet) = props.ctx.pushPage(AppTab.Targets, obsIds.some, none)

        oData.foldMap { case (destAg, draggedIds) =>
          draggedIds match {
            case Left(targetId) if !destAg.targetIds.contains(targetId) =>
              AsterismGroupObsListActions
                .dropTarget(destAg.obsIds, targetId, expandedIds)
                .set(undoCtx)(destAg.some)
            case Right(obsIds) if !destAg.obsIds.intersects(obsIds)     =>
              AsterismGroupObsListActions
                .dropObservations(obsIds, expandedIds, selected, setObsSet)
                .set(undoCtx)(destAg.some)
            case _                                                      => Callback.empty
          }
        }
      }

    def render(props: Props) = {
      implicit val ctx = props.ctx

      val observations   = props.asterismsWithObs.get.observations
      val asterismGroups = props.asterismsWithObs.get.asterismGroups.map(_._2)
      val targetGroupMap = props.asterismsWithObs.get.targetGroups

      val state   = View.fromState($)
      val undoCtx = UndoContext(
        props.undoStacks,
        props.asterismsWithObs
      )

      def renderObsClone(obsIds: ObsIdSet): Option[TagMod] =
        obsIds.single.fold {
          val list        = obsIds.toList
          val div: TagMod = <.div(
            SegmentGroup(
              list.toTagMod(id => Segment(id.show))
            )
          )
          div.some
        }(obsId => observations.get(obsId).map(summ => props.renderObsBadge(summ)))

      val renderClone: Draggable.Render = (provided, snapshot, rubric) =>
        <.div(provided.innerRef,
              provided.draggableProps,
              provided.dragHandleProps,
              props.getDraggedStyle(provided.draggableStyle, snapshot)
        )(
          // we don't need to render clones for the targets, the default is what we want.
          // (We don't include a `renderClone` value in the target `Droppable`.)
          getDraggedIds(rubric.draggableId, props)
            .flatMap {
              case Left(_)       => none
              case Right(obsIds) => renderObsClone(obsIds)
            }
            .getOrElse(<.span("ERROR"))
        )

      val handleDragEnd = onDragEnd(undoCtx, props.expandedIds, props.selected)

      def isTargetSelected(targetId: Target.Id): Boolean =
        props.selected.get.optValue.flatMap(_.left.toOption).exists(_ === targetId)

      def setObsAndTarget(obsId: Option[ObsIdSet], targetId: Option[Target.Id]): Callback =
        props.ctx.pushPage(AppTab.Targets, obsId, targetId)

      def setSelectedPanelToTarget(targetId: Target.Id): Callback =
        setObsAndTarget(none, targetId.some) >>
          props.selected.set(SelectedPanel.editor(targetId.asLeft))

      def isObsSelected(obsId: Observation.Id): Boolean =
        props.selected.get.optValue.flatMap(_.toOption).exists(_.exists(_ === obsId))

      def setSelectedPanelToSet(obsIdSet: ObsIdSet): Callback =
        props.selected.set(SelectedPanel.editor(obsIdSet.asRight))

      def setSelectedPanelToSingle(obsId: Observation.Id): Callback =
        setSelectedPanelToSet(ObsIdSet.one(obsId))

      def setSelectedPanelAndObs(obsId: Observation.Id): Callback =
        setObsAndTarget(ObsIdSet.one(obsId).some, props.focusedTarget) >>
          setSelectedPanelToSingle(obsId)

      def setSelectedPanelAndObsToSet(obsIdSet: ObsIdSet): Callback =
        setObsAndTarget(obsIdSet.some, props.focusedTarget) >>
          setSelectedPanelToSet(obsIdSet)

      def clearSelectedPanelAndObs: Callback =
        setObsAndTarget(none, none) >>
          props.selected.set(SelectedPanel.tree)

      def handleCtrlClick(obsIds: Observation.Id, groupIds: ObsIdSet) =
        props.selected.get.optValue.flatMap(_.toOption).fold(setSelectedPanelAndObs(obsIds)) {
          selectedIds =>
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
        val targets = asterismGroup.targetIds.toList.map(targetGroupMap.get).flatten
        if (targets.isEmpty) "<No Targets>"
        else targets.map(_.targetWithId.target.name).mkString(";")
      }

      def renderAsterismGroup(asterismGroup: AsterismGroup): VdomNode = {
        val obsIds        = asterismGroup.obsIds
        val cgObs         = obsIds.toList.map(id => observations.get(id)).flatten
        // if this group or something in it is selected
        val groupSelected =
          props.selected.get.optValue.flatMap(_.toOption).exists(_.intersects(obsIds))

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
                        onSelect = _ => setSelectedPanelAndObs(obs.id),
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

      def renderTarget(targetGroup: TargetGroup, index: Int): VdomNode = {
        val targetId     = targetGroup.targetWithId.id
        val deleteButton = Button(
          size = Small,
          compact = true,
          clazz = ExploreStyles.DeleteButton |+| ExploreStyles.ObsDeleteButton,
          icon = Icons.Trash,
          onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) =>
            e.preventDefaultCB *>
              e.stopPropagationCB *>
              AsterismGroupObsListActions.targetExistence(targetId).set(undoCtx)(targetGroup.some)
        )

        Draggable(targetId.toString, index) { case (provided, snapshot, _) =>
          <.div(
            provided.innerRef,
            provided.draggableProps,
            props.getDraggedStyle(provided.draggableStyle, snapshot),
            ^.onClick ==> { _ => setSelectedPanelToTarget(targetId) }
          )(
            <.span(provided.dragHandleProps)(
              Card(raised = isTargetSelected(targetId))(ExploreStyles.ObsBadge)(
                CardContent(
                  CardHeader(
                    <.div(
                      ExploreStyles.ObsBadgeHeader,
                      <.div(
                        ExploreStyles.ObsBadgeTargetAndId,
                        <.div(targetGroup.targetWithId.target.name.value),
                        <.div(ExploreStyles.ObsBadgeId, s"[${targetId.value.value.toHexString}]"),
                        deleteButton
                      )
                    )
                  )
                )
              )
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
            Button(
              onClick = props.ctx.pushPage(AppTab.Targets, none, none) >>
                props.selected.set(SelectedPanel.summary),
              clazz = ExploreStyles.ButtonSummary
            )(
              Icons.ListIcon.clazz(ExploreStyles.PaddedRightIcon),
              "Target Summary"
            )
          ),
          ReflexContainer()(
            List[TagMod](
              ReflexElement(minSize = 36, clazz = ExploreStyles.ObsTreeSection)(
                Header(block = true, clazz = ExploreStyles.ObsTreeHeader)("Asterisms"),
                <.div(ExploreStyles.ObsTree)(
                  <.div(ExploreStyles.ObsScrollTree)(
                    asterismGroups.toTagMod(renderAsterismGroup)
                  )
                )
              ),
              ReflexSplitter(propagate = true),
              ReflexElement(minSize = 36, clazz = ExploreStyles.ObsTreeSection, withHandle = true)(
                ReflexWithHandle(reflexProvided =>
                  Droppable("target-list") { case (provided, _) =>
                    <.div(provided.innerRef, provided.droppableProps)(
                      ReflexHandle(provided = reflexProvided)(
                        Header(block = true, clazz = ExploreStyles.ObsTreeHeader)("Target Library")
                      ),
                      <.div(ExploreStyles.ObsTree)(
                        <.div(ExploreStyles.ObsScrollTree)(
                          targetGroupMap.toList.map(_._2).zipWithIndex.toTagMod { case (tg, idx) =>
                            renderTarget(tg, idx)
                          },
                          provided.placeholder
                        )
                      )
                    )
                  }
                )
              )
            ).toTagMod
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
      val expandedIds      = $.props.expandedIds
      val selected         = $.props.selected

      val selectedAG = $.props.focusedObsSet
        .flatMap(idSet => asterismGroups.find { case (key, _) => idSet.subsetOf(key) })
        .map(_._2)

      def replacePage(oid: Option[ObsIdSet], tid: Option[Target.Id]): Callback =
        $.props.ctx.replacePage(AppTab.Targets, oid, tid)

      val obsMissing    = $.props.focusedObsSet.nonEmpty && selectedAG.isEmpty
      val targetMissing =
        $.props.focusedTarget.fold(false)(tid => !asterismsWithObs.targetGroups.contains(tid))

      val (newFocusObs, newFocusTarget, needNewPage) = (obsMissing, targetMissing) match {
        case (true, _) => (none, none, true)
        case (_, true) => ($.props.focusedObsSet, none, true)
        case _         => ($.props.focusedObsSet, $.props.focusedTarget, false)
      }

      val unfocus = if (needNewPage) replacePage(newFocusObs, newFocusTarget) else Callback.empty

      val setAndGetSelected: CallbackTo[Option[AsterismGroup]] = selected.get match {
        case Uninitialized =>
          val (optTorObs, optAG) =
            (newFocusObs, newFocusTarget) match {
              case (Some(obsId), _)    => (obsId.asRight.some, selectedAG)
              case (_, Some(targetId)) => (targetId.asLeft.some, none)
              case _                   => (none, none)
            }
          selected
            .set(optTorObs.fold(SelectedPanel.tree[TargetOrObsSet])(SelectedPanel.editor))
            .as(optAG)
        case Editor(tOrOs) =>
          tOrOs match {
            case Left(_)       => CallbackTo(none)
            case Right(obsIds) => CallbackTo(asterismGroups.find(_._1.intersects(obsIds)).map(_._2))
          }
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
