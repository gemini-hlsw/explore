// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.Order._
import cats.effect.IO
import cats.syntax.all._
import crystal.react.View
import crystal.react.hooks._
import crystal.react.implicits._
import explore.Icons
import explore.common.AsterismQueries._
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.implicits._
import explore.model.AsterismGroup
import explore.model.EmptySiderealTarget
import explore.model.Focused
import explore.model.ObsIdSet
import explore.model.TargetWithObs
import explore.model.enums.AppTab
import explore.undo._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import mouse.boolean._
import queries.common.TargetQueriesGQL
import queries.schemas.implicits._
import react.beautifuldnd._
import react.common._
import react.common.implicits._
import react.fa.FontAwesomeIcon
import react.fa.given
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
  asterismsWithObs: View[AsterismGroupsWithObs],
  programId:        Program.Id,
  focused:          Focused,
  setSummaryPanel:  Callback,
  expandedIds:      View[SortedSet[ObsIdSet]],
  undoStacks:       View[UndoStacks[IO, AsterismGroupsWithObs]]
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[AsterismGroupObsList](AsterismGroupObsList.component)
    with ViewCommon {
  override val focusedObsSet: Option[ObsIdSet] = focused.obsSet
}

object AsterismGroupObsList {
  type Props = AsterismGroupObsList

  // Would be better respresented as a union type in scala 3
  private type TargetOrObsSet = Either[Target.Id, ObsIdSet]

  private def toggleExpanded(
    obsIds:      ObsIdSet,
    expandedIds: View[SortedSet[ObsIdSet]]
  ): Callback =
    expandedIds.mod { expanded =>
      expanded
        .exists(_ === obsIds)
        .fold(expanded - obsIds, expanded + obsIds)
    }

  private def parseDragId(dragId: String): Option[Either[Target.Id, Observation.Id]] =
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
  private def getDraggedIds(dragId: String, props: Props): Option[TargetOrObsSet] =
    parseDragId(dragId).map {
      case Left(targetId) => targetId.asLeft
      case Right(obsId)   =>
        props.focusedObsSet
          .fold(ObsIdSet.one(obsId)) { selectedIds =>
            if (selectedIds.contains(obsId)) selectedIds
            else ObsIdSet.one(obsId)
          }
          .asRight
    }

  private def onDragEnd(
    props:   Props,
    undoCtx: UndoContext[AsterismGroupsWithObs]
  ): (DropResult, ResponderProvided) => Callback = { (result, _) =>
    implicit val ctx = props.ctx

    val oData = for {
      destination <- result.destination.toOption
      destIds     <- ObsIdSet.fromString.getOption(destination.droppableId)
      draggedIds  <- getDraggedIds(result.draggableId, props)
      destAg      <-
        props.asterismsWithObs.get.asterismGroups.values
          .find(_.obsIds === destIds)
    } yield (destAg, draggedIds)

    def setObsSet(obsIds: ObsIdSet) =
      props.ctx.pushPage(AppTab.Targets, props.programId, Focused(obsIds.some))

    oData.foldMap { case (destAg, draggedIds) =>
      draggedIds match {
        case Left(targetId) if !destAg.targetIds.contains(targetId) =>
          AsterismGroupObsListActions
            .dropTarget(destAg.obsIds, targetId, props.expandedIds)
            .set(undoCtx)(destAg.some)
        case Right(obsIds) if !destAg.obsIds.intersects(obsIds)     =>
          AsterismGroupObsListActions
            .dropObservations(obsIds, props.expandedIds, setObsSet)
            .set(undoCtx)(destAg.some)
        case _                                                      => Callback.empty
      }
    }
  }

  private def insertSiderealTarget(
    programId:     Program.Id,
    undoCtx:       UndoContext[AsterismGroupsWithObs],
    adding:        View[Boolean],
    setTargetPage: Target.Id => Option[Target.Id] => IO[Unit]
  )(implicit
    ctx:           AppContextIO
  ): IO[Unit] =
    adding.async.set(true) >>
      TargetQueriesGQL.CreateTargetMutation
        .execute(EmptySiderealTarget.toCreateTargetInput(programId))
        // .void
        .flatMap { data =>
          val targetId = data.createTarget.target.id

          AsterismGroupObsListActions
            .targetExistence(targetId, setTargetPage(targetId))
            .set(undoCtx)(
              TargetWithObs(EmptySiderealTarget, SortedSet.empty).some
            )
            .to[IO]
        }
        .guarantee(adding.async.set(false))

  protected val component = ScalaFnComponent
    .withHooks[Props]
    .useState(false)                          // dragging
    .useStateView(false)                      // adding
    .useRefBy((props, _, _) => props.focused) // focusedRef
    // Make sure we see latest focused value in callbacks
    .useEffectBy((props, _, _, focusedRef) => focusedRef.set(props.focused))
    .useEffectOnMountBy { (props, _, _, _) =>
      val asterismsWithObs = props.asterismsWithObs.get
      val asterismGroups   = asterismsWithObs.asterismGroups
      val expandedIds      = props.expandedIds

      val selectedAG = props.focusedObsSet
        .flatMap(idSet => asterismGroups.find { case (key, _) => idSet.subsetOf(key) })
        .map(_._2)

      def replacePage(focused: Focused): Callback =
        props.ctx.replacePage(AppTab.Targets, props.programId, focused)

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
    .render { (props, dragging, adding, focusedRef) =>
      implicit val ctx = props.ctx

      val observations     = props.asterismsWithObs.get.observations
      val asterismGroups   = props.asterismsWithObs.get.asterismGroups.map(_._2)
      val targetWithObsMap = props.asterismsWithObs.get.targetsWithObs

      val undoCtx = UndoContext(props.undoStacks, props.asterismsWithObs)

      def isObsSelected(obsId: Observation.Id): Boolean =
        props.focused.obsSet.exists(_.contains(obsId))

      def isTargetSelected(targetId: Target.Id): Boolean =
        props.focused.obsSet.isEmpty && props.focused.target.exists(_ === targetId)

      def setFocused(focused: Focused): Callback =
        props.ctx.pushPage(AppTab.Targets, props.programId, focused)

      val setTargetPage: Target.Id => Option[Target.Id] => IO[Unit] =
        targetId =>
          newTargetId =>
            focusedRef.getAsync >>= (currentFocused =>
              newTargetId match { // When deleting, focus away only if focus was on deleted target.
                case None if !currentFocused.target.contains(targetId) => IO.unit
                case other                                             => setFocused(Focused(target = other)).to[IO]
              }
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
        <.div(
          provided.innerRef,
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

      val handleDragEnd = onDragEnd(props, undoCtx)

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

      def renderTarget(
        targetId:      Target.Id,
        targetWithObs: TargetWithObs,
        setPage:       Option[Target.Id] => IO[Unit],
        index:         Int
      )(implicit
        ctx:           AppContextIO
      ): VdomNode = {
        val deleteButton = Button(
          size = Small,
          compact = true,
          clazz = ExploreStyles.DeleteButton |+| ExploreStyles.ObsDeleteButton,
          // icon = Icons.Trash,
          onClickE = (e: ReactMouseEvent, _: Button.ButtonProps) =>
            e.preventDefaultCB >>
              e.stopPropagationCB >>
              AsterismGroupObsListActions
                .targetExistence(targetId, setPage)
                .set(undoCtx)(none)
        )

        Draggable(targetId.toString, index) { case (provided, snapshot, _) =>
          <.div(
            provided.innerRef,
            provided.draggableProps,
            props.getDraggedStyle(provided.draggableStyle, snapshot),
            ^.onClick ==> { _ => setFocused(Focused.target(targetId)) }
          )(
            <.span(provided.dragHandleProps)(
              Card(raised = isTargetSelected(targetId))(ExploreStyles.ObsBadge)(
                CardContent(
                  CardHeader(
                    <.div(
                      ExploreStyles.ObsBadgeHeader,
                      <.div(
                        ExploreStyles.ObsBadgeTargetAndId,
                        <.div(targetWithObs.target.name.value),
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
        onDragStart = (_: DragStart, _: ResponderProvided) => dragging.setState(true),
        onDragEnd =
          (result, provided) => dragging.setState(false) >> handleDragEnd(result, provided)
      )(
        <.div(ExploreStyles.ObsTreeWrapper)(
          <.div(ExploreStyles.TreeToolbar)(
            UndoButtons(undoCtx, size = Mini)
          ),
          <.div(
            Button(
              onClick = setFocused(Focused.None) >> props.setSummaryPanel,
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
                        Header(block = true, clazz = ExploreStyles.ObsTreeHeader)(
                          "Target Library",
                          Button(
                            size = Mini,
                            compact = true,
                            positive = true,
                            icon = Icons.New,
                            content = "Sidereal",
                            disabled = adding.get,
                            loading = adding.get,
                            onClick = insertSiderealTarget(
                              props.programId,
                              undoCtx,
                              adding,
                              setTargetPage
                            ).runAsync
                          )
                        )
                      ),
                      <.div(ExploreStyles.ObsTree)(
                        <.div(ExploreStyles.ObsScrollTree)(
                          targetWithObsMap.toList.zipWithIndex.map { case ((tid, t), idx) =>
                            renderTarget(tid, t, setTargetPage(tid), idx)
                          }.toTagMod,
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
