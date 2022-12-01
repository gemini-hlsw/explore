// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import clue.TransactionalClient
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import explore.Icons
import explore.common.AsterismQueries.*
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.model.AppContext
import explore.model.AsterismGroup
import explore.model.Focused
import explore.model.ObsIdSet
import explore.model.TargetWithObs
import explore.model.enums.AppTab
import explore.undo.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import mouse.boolean.*
import org.typelevel.log4cats.Logger
import react.beautifuldnd.*
import react.common.ReactFnProps
import react.fa.FontAwesomeIcon
import react.semanticui.elements.button.Button
import react.semanticui.elements.header.Header
import react.semanticui.elements.segment.Segment
import react.semanticui.elements.segment.SegmentGroup
import react.semanticui.shorthand.*
import react.semanticui.sizes.*
import react.semanticui.views.card.*

import scala.collection.immutable.SortedSet

case class AsterismGroupObsList(
  asterismsWithObs: View[AsterismGroupsWithObs],
  programId:        Program.Id,
  focused:          Focused,
  setSummaryPanel:  Callback,
  expandedIds:      View[SortedSet[ObsIdSet]],
  undoCtx:          UndoContext[AsterismGroupsWithObs]
) extends ReactFnProps[AsterismGroupObsList](AsterismGroupObsList.component)
    with ViewCommon:
  override val focusedObsSet: Option[ObsIdSet] = focused.obsSet

object AsterismGroupObsList:
  private type Props = AsterismGroupObsList

  private def toggleExpanded(
    obsIds:      ObsIdSet,
    expandedIds: View[SortedSet[ObsIdSet]]
  ): Callback =
    expandedIds.mod { expanded =>
      expanded
        .exists(_ === obsIds)
        .fold(expanded - obsIds, expanded + obsIds)
    }

  private def parseDragId(dragId: String): Option[Observation.Id] =
    Observation.Id
      .parse(dragId)

  /**
   * When we're dragging an observation, we can either be dragging a single observation or a group
   * of them. If we have a selection, and the drag id is part of the selection, we drag all the
   * items in the selection. However, the user may have something selected, but be dragging
   * something that is NOT in the selection - in which case we just drag the individual item.
   */
  private def getDraggedIds(dragId: String, props: Props): Option[ObsIdSet] =
    parseDragId(dragId).map { obsId =>
      props.focusedObsSet
        .fold(ObsIdSet.one(obsId)) { selectedIds =>
          if (selectedIds.contains(obsId)) selectedIds
          else ObsIdSet.one(obsId)
        }
    }

  private def onDragEnd(
    props: Props,
    ctx:   AppContext[IO]
  ): (DropResult, ResponderProvided) => Callback = { (result, _) =>
    import ctx.given

    val oData = for {
      destination <- result.destination.toOption
      destIds     <- ObsIdSet.fromString.getOption(destination.droppableId)
      draggedIds  <- getDraggedIds(result.draggableId, props)
      destAg      <-
        props.asterismsWithObs.get.asterismGroups.values.find(_.obsIds === destIds)
      srcAg       <- props.asterismsWithObs.get.asterismGroups.findContainingObsIds(draggedIds)
    } yield (destAg, draggedIds, srcAg.obsIds)

    def setObsSet(obsIds: ObsIdSet) =
      // if focused is empty, we're looking at the target summary table and don't want to
      // switch to editing because of drag and drop
      if (props.focused.isEmpty) Callback.empty
      else ctx.pushPage(AppTab.Targets, props.programId, Focused(obsIds.some))

    oData.foldMap { (destAg, draggedIds, srcIds) =>
      if (destAg.obsIds.intersects(draggedIds)) Callback.empty
      else
        AsterismGroupObsListActions
          .dropObservations(draggedIds, srcIds, destAg.obsIds, props.expandedIds, setObsSet)
          .set(props.undoCtx)(destAg.some)
    }
  }

  private val component = ScalaFnComponent
    .withHooks[Props]
    .useContext(AppContext.ctx)
    .useState(false) // dragging
    .useEffectOnMountBy { (props, ctx, _) =>
      val asterismsWithObs = props.asterismsWithObs.get
      val asterismGroups   = asterismsWithObs.asterismGroups
      val expandedIds      = props.expandedIds

      val selectedAG = props.focusedObsSet
        .flatMap(idSet => asterismGroups.find { case (key, _) => idSet.subsetOf(key) })
        .map(_._2)

      def replacePage(focused: Focused): Callback =
        ctx.replacePage(AppTab.Targets, props.programId, focused)

      val obsMissing    = props.focused.obsSet.nonEmpty && selectedAG.isEmpty
      val targetMissing =
        props.focused.target.fold(false)(tid =>
          !asterismsWithObs.targetsWithObs.keySet.contains(tid)
        )

      val (newFocused, needNewPage) = (obsMissing, targetMissing) match {
        case (true, _) => (Focused.None, true)
        case (_, true) => (props.focused.withoutTarget, true)
        case _         => (props.focused, false)
      }

      val unfocus = if (needNewPage) replacePage(newFocused) else Callback.empty

      val expandSelected = selectedAG.foldMap(ag => expandedIds.mod(_ + ag.obsIds))

      def cleanupExpandedIds =
        expandedIds.mod(_.filter(asterismGroups.contains))

      for {
        _ <- unfocus
        _ <- expandSelected
        _ <- cleanupExpandedIds
      } yield ()
    }
    .render { (props, ctx, dragging) =>
      import ctx.given

      val observations     = props.asterismsWithObs.get.observations
      val asterismGroups   = props.asterismsWithObs.get.asterismGroups.map(_._2)
      val targetWithObsMap = props.asterismsWithObs.get.targetsWithObs

      def isObsSelected(obsId: Observation.Id): Boolean =
        props.focused.obsSet.exists(_.contains(obsId))

      def setFocused(focused: Focused): Callback =
        ctx.pushPage(AppTab.Targets, props.programId, focused)

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
        <.div(
          provided.innerRef,
          provided.draggableProps,
          provided.dragHandleProps,
          props.getDraggedStyle(provided.draggableStyle, snapshot)
        )(
          getDraggedIds(rubric.draggableId, props)
            .flatMap(renderObsClone)
            .getOrElse(<.span("ERROR"))
        )

      val handleDragEnd = onDragEnd(props, ctx)

      def handleCtrlClick(obsId: Observation.Id, groupIds: ObsIdSet) =
        props.focused.obsSet.fold(setFocused(props.focused.withSingleObs(obsId))) { selectedIds =>
          if (selectedIds.subsetOf(groupIds)) {
            if (selectedIds.contains(obsId))
              setFocused(selectedIds.removeOne(obsId).fold(Focused.None)(props.focused.withObsSet))
            else
              setFocused(props.focused.withObsSet(selectedIds.add(obsId)))
          } else Callback.empty // Not in the same group
        }

      def getAsterismGroupName(asterismGroup: AsterismGroup): String = {
        val targets = asterismGroup.targetIds.toList.map(targetWithObsMap.get).flatten

        if (targets.isEmpty) "<No Targets>"
        else targets.map(_.target.name).mkString(";")
      }

      def renderAsterismGroup(asterismGroup: AsterismGroup): VdomNode = {
        val obsIds        = asterismGroup.obsIds
        val cgObs         = obsIds.toList.map(id => observations.get(id)).flatten
        // if this group or something in it is selected
        val groupSelected = props.focusedObsSet.exists(_.subsetOf(obsIds))

        val icon: FontAwesomeIcon = props.expandedIds.get
          .exists(_ === obsIds)
          .fold(Icons.ChevronDown, Icons.ChevronRight)
          .addModifiers(
            Seq(
              ^.cursor.pointer,
              ^.onClick ==> { (e: ReactEvent) =>
                e.stopPropagationCB >>
                  toggleExpanded(obsIds, props.expandedIds)
                    .asEventDefault(e)
                    .void
              }
            )
          )
          .withFixedWidth()

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
                    Option.when(!dragging.value)(ExploreStyles.UnselectedObsTreeGroup)
                  )
                  .orEmpty
              )(
                ^.cursor.pointer,
                ^.onClick --> setFocused(props.focused.withObsSet(obsIds))
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
                        onSelect = obsId => setFocused(props.focused.withSingleObs(obsId)),
                        onCtrlClick = _ => handleCtrlClick(obs.id, obsIds),
                        ctx
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
        onDragStart = (_: DragStart, _: ResponderProvided) => dragging.setState(true),
        onDragEnd =
          (result, provided) => dragging.setState(false) >> handleDragEnd(result, provided)
      )(
        <.div(ExploreStyles.ObsTreeWrapper)(
          <.div(ExploreStyles.TreeToolbar)(
            UndoButtons(props.undoCtx, size = Mini)
          ),
          <.div(
            Button(
              onClick = setFocused(Focused.None) >> props.setSummaryPanel,
              clazz = ExploreStyles.ButtonSummary
            )(
              Icons.ListIcon.withClass(ExploreStyles.PaddedRightIcon),
              "Target Summary"
            )
          ),
          <.div(ExploreStyles.ObsTree)(
            <.div(ExploreStyles.ObsScrollTree)(
              asterismGroups.toTagMod(renderAsterismGroup)
            )
          )
        )
      )
    }
