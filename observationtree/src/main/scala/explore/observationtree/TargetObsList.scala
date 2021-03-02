// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats._
import cats.effect.IO
import cats.syntax.all._
import clue.TransactionalClient
import crystal.react.implicits._
import eu.timepit.refined.types.numeric.PosLong
import explore.AppCtx
import explore.GraphQLSchemas.ObservationDB
import explore.GraphQLSchemas.ObservationDB.Types._
import explore.Icons
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.components.undo.UndoRegion
import explore.data.KeyedIndexedList
import explore.implicits._
import explore.model.Constants
import explore.model.Focused
import explore.model.Focused._
import explore.model.ObsSummary
import explore.model.TargetViewExpandedIds
import explore.observationtree.ObsBadge
import explore.optics.GetAdjust
import explore.optics._
import explore.undo.KIListMod
import explore.undo.Undoer
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Asterism
import lucuma.core.model.Observation
import lucuma.core.model.Target
import monocle.Getter
import monocle.function.Field1.first
import monocle.macros.Lenses
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
import react.semanticui.views.card.Card
import react.semanticui.views.card.CardContent

import scala.collection.immutable.SortedSet
import scala.util.Random

import TargetObsQueries._

final case class TargetObsList(
  objectsWithObs: View[TargetsAndAsterismsWithObs],
  focused:        View[Option[Focused]],
  expandedIds:    View[TargetViewExpandedIds]
) extends ReactProps[TargetObsList](TargetObsList.component)

object TargetObsList {
  type Props = TargetObsList

  @Lenses
  case class State(dragging: Boolean = false)

  val obsListMod      =
    new KIListMod[IO, ObsAttached, Observation.Id](ObsAttached.id)
  val targetListMod   = new KIListMod[IO, TargetIdName, Target.Id](TargetIdName.id)
  val asterismListMod = new KIListMod[IO, AsterismIdName, Asterism.Id](AsterismIdName.id)

  class Backend($ : BackendScope[Props, State]) {

    def moveObs(obsId: Observation.Id, to: ObjectId)(implicit
      c:               TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      (to match {
        case Left(targetId)    => ShareTargetWithObs.execute(targetId, obsId)
        case Right(asterismId) => ShareAsterismWithObs.execute(asterismId, obsId)
      }).void

    def updateObs(input: EditObservationInput)(implicit
      c:                 TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      UpdateObservationMutation.execute(input).void

    def insertTarget(target: TargetIdName)(implicit
      c:                     TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      AddTarget
        .execute(target.id, target.name.value)
        .handleErrorWith { _ =>
          UndeleteTarget.execute(target.id)
        }
        .void

    def removeTarget(id: Target.Id)(implicit
      c:                 TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      RemoveTarget.execute(id).void

    def insertAsterism(asterism: AsterismIdName)(implicit
      c:                         TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      AddAsterism
        .execute(asterism.id, asterism.name.value)
        .handleErrorWith { _ =>
          UndeleteAsterism.execute(asterism.id)
        }
        .void

    def removeAsterism(id: Asterism.Id)(implicit
      c:                   TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      RemoveAsterism.execute(id).void

    def shareTargetWithAsterism(targetId: Target.Id, asterismId: Asterism.Id)(implicit
      c:                                  TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      ShareTargetWithAsterisms.execute(targetId, asterismId).void

    private def unshareTargetWithAsterism(targetId: Target.Id, asterismId: Asterism.Id)(implicit
      c:                                            TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      UnshareTargetWithAsterisms.execute(targetId, asterismId).void

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
    )(implicit
      c:                     TransactionalClient[IO, ObservationDB]
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
    )(implicit
      c:           TransactionalClient[IO, ObservationDB]
    ): (DropResult, ResponderProvided) => IO[Unit] =
      (result, _) =>
        $.propsIn[IO] >>= { props =>
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
                      // Target dragged to asterism.
                      Asterism.Id.parse(destination.droppableId) match {
                        case Some(asterismId) =>
                          props.objectsWithObs.get.targets
                            .getElement(targetId)
                            .foldMap(target =>
                              expandedIds
                                .zoom(TargetViewExpandedIds.asterismIds)
                                .mod(_ + asterismId) >>
                                addTargetToAsterism(props.objectsWithObs,
                                                    target,
                                                    asterismId,
                                                    setter
                                )
                            )
                        case None             => IO.unit
                      }
                    case None           => IO.unit
                  }
              })
            )
            .orEmpty
        }

    private def setTargetWithIndex(
      objectsWithObs:        View[TargetsAndAsterismsWithObs],
      focused:               View[Option[Focused]],
      targetId:              Target.Id,
      targetWithIndexSetter: Adjuster[TargetList, targetListMod.ElemWithIndex],
      nextToFoucs:           Option[TargetIdName]
    )(implicit
      c:                     TransactionalClient[IO, ObservationDB]
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
    )(implicit
      c:              TransactionalClient[IO, ObservationDB]
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

    protected def newTarget(setter: Undoer.Setter[IO, TargetsAndAsterismsWithObs])(implicit
      c:                            TransactionalClient[IO, ObservationDB]
    ): IO[Unit] = {
      // Temporary measure until we have id pools.
      val newTarget = IO(Random.nextInt()).map(int =>
        TargetIdName(Target.Id(PosLong.unsafeFrom(int.abs.toLong + 1)), Constants.UnnamedTarget)
      )

      $.propsIn[IO] >>= { props =>
        newTarget >>= { target =>
          val mod = targetMod(setter, props.objectsWithObs, props.focused, target.id, none)
          mod(targetListMod.upsert(target, props.objectsWithObs.get.targets.length))
        }
      }
    }

    protected def deleteTarget(
      targetId:      Target.Id,
      setter:        Undoer.Setter[IO, TargetsAndAsterismsWithObs],
      focusOnDelete: Option[TargetIdName]
    )(implicit
      c:             TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      $.propsIn[IO] >>= { props =>
        val mod = targetMod(setter, props.objectsWithObs, props.focused, targetId, focusOnDelete)
        mod(targetListMod.delete)
      }

    protected def newAsterism(setter: Undoer.Setter[IO, TargetsAndAsterismsWithObs])(implicit
      c:                              TransactionalClient[IO, ObservationDB]
    ): IO[Unit] = {
      // Temporary measure until we have id pools.
      val newAsterism = IO(Random.nextInt()).map(int =>
        AsterismIdName(Asterism.Id(PosLong.unsafeFrom(int.abs.toLong + 1)),
                       KeyedIndexedList.empty,
                       Constants.UnnamedAsterism
        )
      )

      $.propsIn[IO] >>= { props =>
        newAsterism >>= { asterism =>
          val mod = asterismMod(setter, props.objectsWithObs, props.focused, asterism.id, none)
          mod(asterismListMod.upsert(asterism, props.objectsWithObs.get.asterisms.length))
        }
      }
    }

    protected def deleteAsterism(
      asterismId:    Asterism.Id,
      setter:        Undoer.Setter[IO, TargetsAndAsterismsWithObs],
      focusOnDelete: Option[AsterismIdName]
    )(implicit
      c:             TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      $.propsIn[IO] >>= { props =>
        val mod =
          asterismMod(setter, props.objectsWithObs, props.focused, asterismId, focusOnDelete)
        mod(asterismListMod.delete)
      }

    private def setAsterismWithIndex(
      objectsWithObs:          View[TargetsAndAsterismsWithObs],
      focused:                 View[Option[Focused]],
      asterismId:              Asterism.Id,
      asterismWithIndexSetter: Adjuster[AsterismList, asterismListMod.ElemWithIndex],
      nextToFoucs:             Option[AsterismIdName]
    )(implicit
      c:                       TransactionalClient[IO, ObservationDB]
    ): asterismListMod.ElemWithIndex => IO[Unit] = { asterismWithIndex =>
      val view = objectsWithObs
        .zoom(TargetsAndAsterismsWithObs.asterisms)

      // 1) Update internal model
      view
        .mod(asterismWithIndexSetter.set(asterismWithIndex)) >>
        // 2) Send mutation & adjust focus
        asterismWithIndex.fold(
          focused.set(nextToFoucs.map(f => FocusedAsterism(f.id))) *> removeAsterism(asterismId)
        ) { case (asterism, _) =>
          insertAsterism(asterism) *> focused.set(FocusedAsterism(asterismId).some)
        }
    }

    private def asterismMod(
      setter:         Undoer.Setter[IO, TargetsAndAsterismsWithObs],
      objectsWithObs: View[TargetsAndAsterismsWithObs],
      focused:        View[Option[Focused]],
      asterismId:     Asterism.Id,
      focusOnDelete:  Option[AsterismIdName]
    )(implicit
      c:              TransactionalClient[IO, ObservationDB]
    ): asterismListMod.Operation => IO[Unit] = {
      val asterismWithId: GetAdjust[AsterismList, asterismListMod.ElemWithIndex] =
        asterismListMod.withKey(asterismId)

      setter
        .mod[asterismListMod.ElemWithIndex](
          objectsWithObs.get,
          TargetsAndAsterismsWithObs.asterisms
            .composeGetter(asterismWithId.getter)
            .get,
          setAsterismWithIndex(objectsWithObs,
                               focused,
                               asterismId,
                               asterismWithId.adjuster,
                               focusOnDelete
          )
        )
    }

    private def setAsterismTargetWithIndex(
      objectsWithObs:        View[TargetsAndAsterismsWithObs],
      targetId:              Target.Id,
      asterismId:            Asterism.Id,
      targetWithIndexSetter: Adjuster[TargetList, targetListMod.ElemWithIndex]
    )(implicit
      c:                     TransactionalClient[IO, ObservationDB]
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
    )(implicit
      c:              TransactionalClient[IO, ObservationDB]
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
    )(implicit
      c:              TransactionalClient[IO, ObservationDB]
    ): IO[Unit] = {
      val mod = asterismTargetMod(setter, objectsWithObs, target.id, asterismId)
      mod(
        targetListMod.upsert(
          target,
          objectsWithObs.get.asterisms.getElement(asterismId).foldMap(_.targets.length)
        )
      )
    }

    protected def deleteTargetFromAsterism(
      targetId:   Target.Id,
      asterismId: Asterism.Id,
      setter:     Undoer.Setter[IO, TargetsAndAsterismsWithObs]
    )(implicit
      c:          TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      $.propsIn[IO] >>= { props =>
        val mod = asterismTargetMod(setter, props.objectsWithObs, targetId, asterismId)
        mod(targetListMod.delete)
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

    def render(props: Props, state: State): VdomElement = AppCtx.withCtx { implicit ctx =>
      val observations = props.objectsWithObs.get.obs
      val obsByObject  = observations.toList.groupBy(_.attached)

      val targets        = props.objectsWithObs.get.targets
      val targetsWithIdx = targets.toList.zipWithIndex

      val asterisms    = props.objectsWithObs.get.asterisms.toList
      val asterismIds  = asterisms.map(_.id)
      val asterismIdxs = asterisms.zipWithIndex

      def renderObsBadge(obs: ObsAttached): TagMod =
        ObsBadge(
          ObsSummary(id = obs.id,
                     name = obs.name,
                     observationTarget = None
          ), // FIXME Add the target id
          ObsBadge.Layout.ConfAndConstraints,
          selected = props.focused.get.exists(_ === FocusedObs(obs.id))
        )

      def renderObsBadgeItem(obs: ObsAttached, idx: Int): TagMod =
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
                  props.focused.set(FocusedObs(obs.id).some).runAsyncCB
              }
            )(
              <.span(provided.dragHandleProps)(
                renderObsBadge(obs)
              )
            )
          }
        )

      val renderClone: Draggable.Render =
        (provided, snapshot, rubric) => {
          <.div(provided.innerRef,
                provided.draggableProps,
                provided.dragHandleProps,
                getDraggedStyle(provided.draggableStyle, snapshot)
          )(
            (Target.Id
              .parse(rubric.draggableId)
              .toRight(Observation.Id.parse(rubric.draggableId)) match {
              case Right(targetId)   =>
                targets
                  .getElement(targetId)
                  .map(target => Card(raised = true)(CardContent(target.name.value)).vdomElement)
              case Left(Some(obsId)) => observations.getElement(obsId).map(renderObsBadge)
              case _                 => none
            }).getOrElse(<.span("ERROR"))
          )
        }

      UndoRegion[TargetsAndAsterismsWithObs] { undoCtx =>
        val handleDragEnd = onDragEnd(undoCtx.setter, props.expandedIds)

        DragDropContext(
          onDragStart = (_: DragStart, _: ResponderProvided) => $.setStateL(State.dragging)(true),
          onDragEnd = (result, provided) =>
            $.setStateL(State.dragging)(false) >> handleDragEnd(result, provided).runAsyncCB
        )(
          <.div(ExploreStyles.ObsTreeWrapper)(
            <.div(ExploreStyles.TreeToolbar)(
              <.div(
                Button(size = Mini, compact = true, onClick = newTarget(undoCtx.setter).runAsyncCB)(
                  Icons.New.size(Small).fitted(true),
                  " Target"
                ),
                Button(size = Mini,
                       compact = true,
                       onClick = newAsterism(undoCtx.setter).runAsyncCB
                )(
                  Icons.New.size(Small).fitted(true),
                  " Asterism"
                )
              ),
              UndoButtons(props.objectsWithObs.get, undoCtx, size = Mini)
            ),
            <.div(ExploreStyles.ObsTree)(
              <.div(ExploreStyles.ObsScrollTree)(
                targetsWithIdx.toTagMod { case (target, targetIdx) =>
                  val targetId      = target.id
                  val nextToSelect  = targetsWithIdx.find(_._2 === targetIdx + 1).map(_._1)
                  val prevToSelect  = targetsWithIdx.find(_._2 === targetIdx - 1).map(_._1)
                  val focusOnDelete = nextToSelect.orElse(prevToSelect)

                  val targetObs = obsByObject.get(targetId.asLeft).orEmpty

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

                  Droppable(targetId.toString, renderClone = renderClone) {
                    case (provided, snapshot) =>
                      // To implement "copy-drag", we use suggestion from
                      // https://github.com/atlassian/react-beautiful-dnd/issues/216#issuecomment-586266295
                      val shouldRenderClone =
                        snapshot.draggingFromThisWith.exists(
                          _ === targetId.toString
                        )

                      val targetHeader =
                        <.span(ExploreStyles.ObsTreeGroupHeader)(
                          <.span(ExploreStyles.TargetLabelTitle)(
                            opIcon,
                            target.name.value
                          ),
                          Button(
                            size = Small,
                            compact = true,
                            clazz = ExploreStyles.DeleteButton |+| ExploreStyles.JustifyRight,
                            onClickE = (e: ReactMouseEvent, _: ButtonProps) =>
                              e.stopPropagationCB *>
                                deleteTarget(targetId, undoCtx.setter, focusOnDelete).runAsyncCB
                          )(
                            Icons.Trash
                          ),
                          <.span(ExploreStyles.ObsCount, s"${targetObs.length} Obs")
                        )

                      <.div(
                        provided.innerRef,
                        provided.droppableProps,
                        getListStyle(
                          snapshot.draggingOverWith.exists(id => Observation.Id.parse(id).isDefined)
                        )
                      )(
                        Segment(
                          vertical = true,
                          clazz = ExploreStyles.ObsTreeGroup
                            |+| Option
                              .when(
                                memberObsSelected || props.focused.get
                                  .exists(_ === FocusedTarget(targetId))
                              )(ExploreStyles.SelectedObsTreeGroup)
                              .orElse(
                                Option.when(!state.dragging)(
                                  ExploreStyles.UnselectedObsTreeGroup
                                )
                              )
                              .orEmpty
                        )(
                          ^.cursor.pointer,
                          ^.onClick --> props.focused
                            .set(FocusedTarget(targetId).some)
                            .runAsyncCB
                        )(
                          if (asterisms.isEmpty || shouldRenderClone)
                            targetHeader
                          else
                            Draggable(targetId.toString, -1) {
                              case (targetProvided, targetSnapshot, _) =>
                                <.span(
                                  targetProvided.innerRef,
                                  targetProvided.draggableProps,
                                  getDraggedStyle(targetProvided.draggableStyle, targetSnapshot),
                                  targetProvided.dragHandleProps
                                )(
                                  targetHeader
                                )
                            },
                          TagMod
                            .when(expandedTargetIds.get.contains(targetId))(
                              targetObs.zipWithIndex.toTagMod(
                                (renderObsBadgeItem _).tupled
                              )
                            ),
                          <.span(provided.placeholder)
                        )
                      )
                  }
                }
              )
            ),
            <.div(ExploreStyles.ObsTree)(
              <.div(ExploreStyles.ObsScrollTree)(
                asterisms.toTagMod { asterism =>
                  val asterismId    = asterism.id
                  val currIdx       = asterismIds.indexOf(asterismId)
                  val nextToSelect  = asterismIdxs.find(_._2 === currIdx + 1).map(_._1)
                  val prevToSelect  = asterismIdxs.find(_._2 === currIdx - 1).map(_._1)
                  val focusOnDelete = nextToSelect.orElse(prevToSelect)

                  val asterismTargets = asterism.targets.toList
                  val asterismObs     = obsByObject.get(asterismId.asRight).orEmpty

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
                            .orElse(
                              Option.when(!state.dragging)(
                                ExploreStyles.UnselectedObsTreeGroup
                              )
                            )
                            .orEmpty
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
                            clazz = ExploreStyles.DeleteButton |+| ExploreStyles.JustifyRight,
                            onClickE = (e: ReactMouseEvent, _: ButtonProps) =>
                              e.stopPropagationCB >>
                                deleteAsterism(asterismId, undoCtx.setter, focusOnDelete).runAsyncCB
                          )(
                            Icons.Trash
                          ),
                          <.span(ExploreStyles.ObsCount,
                                 s"${asterismTargets.length} Tgts - ${asterismObs.length} Obs"
                          )
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
                                      e.stopPropagationCB >>
                                        deleteTargetFromAsterism(target.id,
                                                                 asterismId,
                                                                 undoCtx.setter
                                        ).runAsyncCB
                                  )(
                                    Icons.Trash
                                  )
                                )
                              )
                            ).when(asterismTargets.nonEmpty),
                            asterismObs.zipWithIndex.toTagMod(
                              (renderObsBadgeItem _).tupled
                            )
                          )
                        ),
                        provided.placeholder
                      )
                    )
                  }
                }
              )
            ).when(asterisms.nonEmpty)
          )
        )
      }
    }
  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .initialState(State())
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
