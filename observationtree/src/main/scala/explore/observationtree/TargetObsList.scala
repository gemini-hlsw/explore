// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats._
import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import eu.timepit.refined.types.numeric.PosLong
import explore.components.ObsBadge
import explore.components.ui.ExploreStyles
import explore.components.undo.{ UndoButtons, UndoRegion }
import explore.implicits._
import explore.model.Focused._
import explore.model.{ Constants, Focused, ObsSummary, TargetViewExpandedIds }
import explore.optics.{ GetAdjust, _ }
import explore.undo.{ KIListMod, Undoer }
import explore.Icons
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.{ Observation, Target }
import monocle.Getter
import monocle.function.Field1.first
import monocle.std.option.some
import mouse.boolean._
import react.beautifuldnd._
import react.common._
import react.common.implicits._
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.Button.ButtonProps
import react.semanticui.elements.icon.Icon
import react.semanticui.elements.segment.Segment
import react.semanticui.elements.segment.SegmentGroup
import react.semanticui.sizes._

import scala.collection.immutable.SortedSet
import scala.util.Random

import TargetObsQueries._
import lucuma.core.model.Asterism

final case class TargetObsList(
  objectsWithObs: View[TargetsAndAsterismsWithObs],
  focused:        View[Option[Focused]],
  expandedIds:    View[TargetViewExpandedIds]
) extends ReactProps[TargetObsList](TargetObsList.component)

object TargetObsList {
  type Props = TargetObsList

  val obsListMod      =
    new KIListMod[IO, ObsAttached, Observation.Id](ObsAttached.id)
  val targetListMod   = new KIListMod[IO, TargetIdName, Target.Id](TargetIdName.id)
  val asterismListMod = new KIListMod[IO, AsterismIdName, Asterism.Id](AsterismIdName.id)

  class Backend($ : BackendScope[Props, Unit]) {

    private def getObjectForObsWithId(
      obsWithIndexGetter: Getter[ObsList, obsListMod.ElemWithIndex]
    ): Getter[TargetsAndAsterismsWithObs, Option[ObjectId]] =
      TargetsAndAsterismsWithObs.obs
        .composeGetter(
          obsWithIndexGetter
            .composeOptionLens(first)
            .composeOptionLens(ObsAttached.attached)
        )

    private def setObjectForObsWithId(
      objectsWithObs:        View[TargetsAndAsterismsWithObs],
      obsId:                 Observation.Id,
      obsWithIndexGetAdjust: GetAdjust[ObsList, obsListMod.ElemWithIndex]
    ): Option[ObjectId] => IO[Unit] =
      objectOpt => {
        val obsTargetAdjuster = obsWithIndexGetAdjust
          .composeOptionLens(first) // Focus on Observation within ElemWithIndex
          .composeOptionLens(ObsAttached.attached)

        val observationsView = objectsWithObs
          .zoom(TargetsAndAsterismsWithObs.obs)

        // 1) Update internal model
        observationsView.mod(obsTargetAdjuster.set(objectOpt)) >>
          // 2) Send mutation
          objectOpt
            .map(newObjectId => moveObs(obsId, newObjectId))
            .orEmpty
      }

    protected def onDragEnd(
      setter:      Undoer.Setter[IO, TargetsAndAsterismsWithObs],
      expandedIds: View[TargetViewExpandedIds]
    ): (DropResult, ResponderProvided) => Callback =
      (result, _) =>
        $.props >>= { props =>
          println(scalajs.js.JSON.stringify(result))
          // We can drag:
          //  - An observation from a target or an asterism to another target or asterism.
          //  - A target into an asterism.

          result.destination.toOption
            .map(destination =>
              (Observation.Id.parse(result.draggableId) match {
                case Some(obsId) =>
                  // Observation dragged to a target or asterism.
                  val obsWithId: GetAdjust[ObsList, obsListMod.ElemWithIndex] =
                    obsListMod.withKey(obsId)

                  val set: Option[ObjectId] => IO[Unit] =
                    setter
                      .set[Option[ObjectId]](
                        props.objectsWithObs.get,
                        getObjectForObsWithId(obsWithId.getter).get,
                        setObjectForObsWithId(props.objectsWithObs, obsId, obsWithId)
                      )

                  Asterism.Id
                    .parse(destination.droppableId)
                    .toRight(Target.Id.parse(destination.droppableId)) match {
                    // Drag to asterism.
                    case Right(newAsterismId)    =>
                      expandedIds.zoom(TargetViewExpandedIds.asterismIds).mod(_ + newAsterismId) >>
                        set(newAsterismId.asRight.some)
                    // Drag to target.
                    case Left(Some(newTargetId)) =>
                      expandedIds.zoom(TargetViewExpandedIds.targetIds).mod(_ + newTargetId) >>
                        set(newTargetId.asLeft.some)
                    case _                       => IO.unit // Report error?
                  }
                case None        =>
                  Target.Id.parse(result.draggableId) match {
                    case Some(targetId) =>
                      Asterism.Id.parse(destination.droppableId) match {
                        case Some(asterismId) =>
                          props.objectsWithObs.get.targets
                            .getElement(targetId)
                            .foldMap(target =>
                              addTargetToAsterism(props.objectsWithObs, target, asterismId, setter)
                            )
                        case None             => IO.unit
                      }
                    case None           => IO.unit
                  }
              }).runAsyncCB
            )
            .getOrEmpty
        }

    private def setTargetWithIndex(
      objectsWithObs:        View[TargetsAndAsterismsWithObs],
      focused:               View[Option[Focused]],
      targetId:              Target.Id,
      targetWithIndexSetter: Adjuster[TargetList, targetListMod.ElemWithIndex],
      nextToFoucs:           Option[TargetIdName]
    ): targetListMod.ElemWithIndex => IO[Unit] =
      targetWithIndex =>
        // 1) Update internal model
        objectsWithObs
          .zoom(TargetsAndAsterismsWithObs.targets)
          .mod(targetWithIndexSetter.set(targetWithIndex)) >>
          // 2) Send mutation & adjust focus
          targetWithIndex.fold(
            focused.set(nextToFoucs.map(f => Focused.FocusedTarget(f.id))) *> removeTarget(targetId)
          ) { case (target, _) =>
            insertTarget(target) *> focused.set(FocusedTarget(targetId).some)
          }

    private def targetMod(
      setter:         Undoer.Setter[IO, TargetsAndAsterismsWithObs],
      objectsWithObs: View[TargetsAndAsterismsWithObs],
      focused:        View[Option[Focused]],
      targetId:       Target.Id,
      focusOnDelete:  Option[TargetIdName]
    ): targetListMod.Operation => IO[Unit] = {
      val targetWithId: GetAdjust[TargetList, targetListMod.ElemWithIndex] =
        targetListMod.withKey(targetId)

      setter
        .mod[targetListMod.ElemWithIndex](
          objectsWithObs.get,
          TargetsAndAsterismsWithObs.targets
            .composeGetter(targetWithId.getter)
            .get,
          setTargetWithIndex(objectsWithObs,
                             focused,
                             targetId,
                             targetWithId.adjuster,
                             focusOnDelete
          )
        )
    }

    protected def newTarget(setter: Undoer.Setter[IO, TargetsAndAsterismsWithObs]): Callback =
      $.props >>= { props =>
        // Temporary measure until we have id pools.
        val newTarget =
          TargetIdName(Target.Id(PosLong.unsafeFrom(Random.nextInt().abs.toLong + 1)),
                       Constants.UnnamedTarget
          )

        val upsert =
          targetListMod
            .upsert(newTarget, props.objectsWithObs.get.targets.length)

        targetMod(setter, props.objectsWithObs, props.focused, newTarget.id, none)(
          upsert
        ).runAsyncCB
      }

    protected def deleteTarget(
      targetId:      Target.Id,
      setter:        Undoer.Setter[IO, TargetsAndAsterismsWithObs],
      focusOnDelete: Option[TargetIdName]
    ): Callback =
      $.props.flatMap { props =>
        targetMod(setter, props.objectsWithObs, props.focused, targetId, focusOnDelete)(
          targetListMod.delete
        ).runAsyncCB
      }

    private def setAsterismTargetWithIndex(
      objectsWithObs:        View[TargetsAndAsterismsWithObs],
      targetId:              Target.Id,
      asterismId:            Asterism.Id,
      targetWithIndexSetter: Adjuster[TargetList, targetListMod.ElemWithIndex]
    ): targetListMod.ElemWithIndex => IO[Unit] =
      targetWithIndex =>
        // 1) Update internal model
        objectsWithObs
          .zoom(TargetsAndAsterismsWithObs.asterisms)
          .zoomGetAdjust(asterismListMod.withKey(asterismId))
          .zoomPrism(some)
          .zoomLens(first)
          .zoom(AsterismIdName.targets)
          .mod(targetWithIndexSetter.set(targetWithIndex)) >>
          // 2) Send mutation & adjust focus
          targetWithIndex.fold(
            unshareTargetWithAsterism(targetId, asterismId)
          ) { case (target, _) =>
            shareTargetWithAsterism(target.id, asterismId)
          }

    private def asterismTargetMod(
      setter:         Undoer.Setter[IO, TargetsAndAsterismsWithObs],
      objectsWithObs: View[TargetsAndAsterismsWithObs],
      targetId:       Target.Id,
      asterismId:     Asterism.Id
    ): targetListMod.Operation => IO[Unit] = {
      val targetWithId: GetAdjust[TargetList, targetListMod.ElemWithIndex] =
        targetListMod.withKey(targetId)

      setter
        .mod[targetListMod.ElemWithIndex](
          objectsWithObs.get,
          TargetsAndAsterismsWithObs.asterisms
            .composeGetter(asterismListMod.withKey(asterismId).getter)
            .map(_.map(_._1.targets).map(targetWithId.getter.get).flatten)
            .get,
          setAsterismTargetWithIndex(objectsWithObs, targetId, asterismId, targetWithId.adjuster)
        )
    }

    protected def addTargetToAsterism(
      objectsWithObs: View[TargetsAndAsterismsWithObs],
      target:         TargetIdName,
      asterismId:     Asterism.Id,
      setter:         Undoer.Setter[IO, TargetsAndAsterismsWithObs]
    ): IO[Unit] =
      asterismTargetMod(setter, objectsWithObs, target.id, asterismId)(
        targetListMod.upsert(
          target,
          objectsWithObs.get.asterisms.getElement(asterismId).foldMap(_.targets.length)
        )
      )

    protected def deleteTargetFromAsterism(
      targetId:   Target.Id,
      asterismId: Asterism.Id,
      setter:     Undoer.Setter[IO, TargetsAndAsterismsWithObs]
    ): Callback =
      $.props.flatMap { props =>
        asterismTargetMod(setter, props.objectsWithObs, targetId, asterismId)(
          targetListMod.delete
        ).runAsyncCB
      }

    def toggleExpanded[A: Eq](
      id:          A,
      expandedIds: View[SortedSet[A]]
    ): Callback =
      expandedIds.mod { expanded =>
        expanded
          .exists(_ === id)
          .fold(expanded - id, expanded + id)
      }.runAsyncCB

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

    def renderObsBadge(focused: View[Option[Focused]])(obs: ObsAttached, idx: Int): TagMod =
      <.div(ExploreStyles.ObsTreeItem)(
        Draggable(obs.id.toString, idx) { case (provided, snapshot, _) =>
          <.div(
            provided.innerRef,
            provided.draggableProps,
            getDraggedStyle(
              provided.draggableStyle,
              snapshot
            ),
            ^.onClick ==> { e: ReactEvent =>
              e.stopPropagationCB >>
                focused.set(FocusedObs(obs.id).some).runAsyncCB
            }
          )(
            <.span(provided.dragHandleProps)(
              ObsBadge(
                ObsSummary(obs.id, obs.name.orEmpty),
                ObsBadge.Layout.ConfAndConstraints,
                selected = focused.get.exists(_ === FocusedObs(obs.id))
              )
            )
          )
        }
      )

    def render(props: Props): VdomElement = try {
      val obsByObject = props.objectsWithObs.get.obs.toList.groupBy(_.attached)

      val targets    = props.objectsWithObs.get.targets.toList
      val targetIds  = targets.map(_.id)
      val targetIdxs = targets.zipWithIndex

      val asterisms = props.objectsWithObs.get.asterisms.toList

      React.Fragment(
        UndoRegion[TargetsAndAsterismsWithObs] { undoCtx =>
          DragDropContext(onDragEnd = onDragEnd(undoCtx.setter, props.expandedIds))(
            <.div(ExploreStyles.ObsTreeWrapper)(
              <.div(ExploreStyles.TreeToolbar)(
                <.div(
                  Button(size = Mini, compact = true, onClick = newTarget(undoCtx.setter))(
                    Icons.New.size(Small).fitted(true)
                  )
                ),
                UndoButtons(props.objectsWithObs.get, undoCtx, size = Mini)
              ),
              <.div(ExploreStyles.ObsTree)(
                <.div(ExploreStyles.ObsScrollTree)(
                  Droppable("targetList") { case (targetListProvided, targetListSnapshot) =>
                    <.div(
                      targetListProvided.innerRef,
                      targetListProvided.droppableProps,
                      getListStyle(targetListSnapshot.isDraggingOver)
                    )(
                      targets.zipWithIndex
                        .toTagMod { case (target, targetIdx) =>
                          val targetId      = target.id
                          val currIdx       = targetIds.indexOf(targetId)
                          val nextToSelect  = targetIdxs.find(_._2 === currIdx + 1).map(_._1)
                          val prevToSelect  = targetIdxs.find(_._2 === currIdx - 1).map(_._1)
                          val focusOnDelete = nextToSelect.orElse(prevToSelect)

                          val targetObs = obsByObject.get(targetId.asLeft).orEmpty
                          val obsCount  = targetObs.length

                          val expandedTargetIds =
                            props.expandedIds.zoom(TargetViewExpandedIds.targetIds)
                          val opIcon            =
                            targetObs.nonEmpty.fold(
                              Icon(
                                "chevron " + expandedTargetIds.get
                                  .exists(_ === targetId)
                                  .fold("down", "right")
                              )(^.cursor.pointer,
                                ^.onClick ==> { e: ReactEvent =>
                                  e.stopPropagationCB >>
                                    toggleExpanded(targetId, expandedTargetIds)
                                      .asEventDefault(e)
                                      .void
                                }
                              ),
                              Icons.ChevronRight
                            )

                          val memberObsSelected = props.focused.get
                            .exists(f => targetObs.map(obs => FocusedObs(obs.id)).exists(f === _))

                          // TODO: Collapse the target while dragging it (and then restore its collapsed state)
                          Draggable(targetId.toString, targetIdx) {
                            case (targetProvided, targetSnapshot, _) =>
                              <.div(
                                targetProvided.innerRef,
                                targetProvided.draggableProps,
                                getDraggedStyle(targetProvided.draggableStyle, targetSnapshot)
                              )(
                                <.span(targetProvided.dragHandleProps)(
                                  Droppable(targetId.toString) { case (provided, snapshot) =>
                                    <.div(
                                      provided.innerRef,
                                      provided.droppableProps,
                                      getListStyle(snapshot.isDraggingOver)
                                    )(
                                      Segment(
                                        vertical = true,
                                        clazz = ExploreStyles.ObsTreeGroup |+| Option
                                          .when(
                                            memberObsSelected || props.focused.get
                                              .exists(_ === FocusedTarget(targetId))
                                          )(ExploreStyles.SelectedObsTreeGroup)
                                          .getOrElse(ExploreStyles.UnselectedObsTreeGroup)
                                      )(
                                        ^.cursor.pointer,
                                        ^.onClick --> props.focused
                                          .set(FocusedTarget(targetId).some)
                                          .runAsyncCB
                                      )(
                                        <.span(ExploreStyles.ObsTreeGroupHeader)(
                                          <.span(
                                            opIcon,
                                            target.name.value,
                                            ExploreStyles.TargetLabelTitle
                                          ),
                                          Button(
                                            size = Small,
                                            compact = true,
                                            clazz =
                                              ExploreStyles.DeleteButton |+| ExploreStyles.JustifyRight,
                                            onClickE = (e: ReactMouseEvent, _: ButtonProps) =>
                                              e.stopPropagationCB *>
                                                deleteTarget(targetId,
                                                             undoCtx.setter,
                                                             focusOnDelete
                                                )
                                          )(
                                            Icons.Delete
                                              .size(Small)
                                              .fitted(true)
                                              .clazz(ExploreStyles.TrashIcon)
                                          ),
                                          <.span(ExploreStyles.ObsCount, s"$obsCount Obs")
                                        ),
                                        TagMod
                                          .when(expandedTargetIds.get.contains(targetId))(
                                            targetObs.zipWithIndex.toTagMod(
                                              (renderObsBadge(props.focused) _).tupled
                                            )
                                          ),
                                        <.span(provided.placeholder)
                                      )
                                    )
                                  }
                                )
                              )
                          }

                        },
                      <.span(targetListProvided.placeholder, ^.display.none)
                    )
                  }
                )
              ),
              <.div(ExploreStyles.ObsTree)(
                <.div(ExploreStyles.ObsScrollTree)(
                  asterisms.toTagMod { asterism =>
                    val asterismId = asterism.id

                    val asterismTargets = asterism.targets.toList
                    val asterismObs     = obsByObject.get(asterismId.asRight).orEmpty
                    val obsCount        = asterismObs.length

                    val expandedAsterismIds =
                      props.expandedIds.zoom(TargetViewExpandedIds.asterismIds)
                    val opIcon              =
                      (asterismObs.nonEmpty || asterismTargets.nonEmpty).fold(
                        Icon(
                          "chevron " + expandedAsterismIds.get
                            .exists(_ === asterismId)
                            .fold("down", "right")
                        )(^.cursor.pointer,
                          ^.onClick ==> { e: ReactEvent =>
                            e.stopPropagationCB >>
                              toggleExpanded(asterismId, expandedAsterismIds)
                                .asEventDefault(e)
                                .void
                          }
                        ),
                        Icons.ChevronRight
                      )

                    val memberObsSelected = props.focused.get
                      .exists(f => asterismObs.map(obs => FocusedObs(obs.id)).exists(f === _))

                    Droppable(asterismId.toString) { case (provided, snapshot) =>
                      <.div(
                        provided.innerRef,
                        provided.droppableProps,
                        getListStyle(snapshot.isDraggingOver)
                      )(
                        Segment(
                          vertical = true,
                          clazz = ExploreStyles.ObsTreeGroup |+|
                            Option
                              .when(
                                memberObsSelected || props.focused.get
                                  .exists(_ === FocusedAsterism(asterismId))
                              )(ExploreStyles.SelectedObsTreeGroup)
                              .getOrElse(ExploreStyles.UnselectedObsTreeGroup)
                        )(
                          ^.cursor.pointer,
                          ^.onClick --> props.focused
                            .set(FocusedAsterism(asterismId).some)
                            .runAsyncCB
                        )(
                          <.span(ExploreStyles.ObsTreeGroupHeader)(
                            <.span(
                              opIcon,
                              asterism.name.value,
                              ExploreStyles.TargetLabelTitle
                            ),
                            Button(
                              size = Small,
                              compact = true,
                              clazz = ExploreStyles.DeleteButton |+| ExploreStyles.JustifyRight
                              // onClickE = (e: ReactMouseEvent, _: ButtonProps) =>
                              //   e.stopPropagationCB *>
                              //     deleteTarget(targetId, undoCtx.setter, focusOnDelete)
                            )(
                              Icons.Delete
                                .size(Small)
                                .fitted(true)
                                .clazz(ExploreStyles.TrashIcon)
                            ),
                            <.span(ExploreStyles.ObsCount, s"$obsCount Obs")
                          ),
                          TagMod.when(expandedAsterismIds.get.contains(asterismId))(
                            <.div(ExploreStyles.ObsTreeItem)(
                              SegmentGroup(
                                asterismTargets.toTagMod(target =>
                                  Segment(basic = true, clazz = ExploreStyles.ObsTreeGroupHeader)(
                                    <.span(ExploreStyles.TargetLabelTitle)(target.name.value),
                                    Button(
                                      size = Small,
                                      compact = true,
                                      clazz =
                                        ExploreStyles.DeleteButton |+| ExploreStyles.JustifyRight,
                                      onClickE = (e: ReactMouseEvent, _: ButtonProps) =>
                                        e.stopPropagationCB *>
                                          deleteTargetFromAsterism(target.id,
                                                                   asterismId,
                                                                   undoCtx.setter
                                          )
                                    )(
                                      Icons.Delete
                                        .size(Small)
                                        .fitted(true)
                                        .clazz(ExploreStyles.TrashIcon)
                                    )
                                  )
                                // )
                                )
                              ).when(asterismTargets.nonEmpty),
                              asterismObs.zipWithIndex.toTagMod(
                                (renderObsBadge(props.focused) _).tupled
                              )
                            )
                          ),
                          provided.placeholder
                        )
                      )
                    }
                  }
                )
              )
            )
          )
        }
      )
    } catch {
      case t: Throwable => t.printStackTrace(); throw t
    }
  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .renderBackend[Backend]
      .componentDidMount { $ =>
        val objectsWithObs      = $.props.objectsWithObs.get
        val expandedTargetIds   = $.props.expandedIds.zoom(TargetViewExpandedIds.targetIds)
        val expandedAsterismIds = $.props.expandedIds.zoom(TargetViewExpandedIds.asterismIds)

        // Expand target or asterism with focused observation
        val expandObservationObject =
          $.props.focused.get
            .collect { case FocusedObs(obsId) =>
              objectsWithObs.obs
                .getElement(obsId)
                .map(_.attached match {
                  case Right(asterismId) => expandedAsterismIds.mod(_ + asterismId)
                  case Left(targetId)    => expandedTargetIds.mod(_ + targetId)
                })
            }
            .flatten
            .orEmpty

        // Remove objects from expanded set which are no longer present.
        val removeTargets =
          (expandedTargetIds.get -- objectsWithObs.targets.toList
            .map(_.id)).toNes
            .map(removedTargetIds => expandedTargetIds.mod(_ -- removedTargetIds.toSortedSet))
            .orEmpty

        val removeAsterisms =
          (expandedAsterismIds.get -- objectsWithObs.asterisms.toList
            .map(_.id)).toNes
            .map(removedAsterismIds => expandedAsterismIds.mod(_ -- removedAsterismIds.toSortedSet))
            .orEmpty

        (expandObservationObject >> removeTargets >> removeAsterisms).runAsyncCB
      }
      .build
}
