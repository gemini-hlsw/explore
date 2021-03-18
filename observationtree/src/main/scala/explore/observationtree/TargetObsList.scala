// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats._
import cats.effect.ContextShift
import cats.effect.IO
import cats.instances.order._
import cats.syntax.all._
import clue.TransactionalClient
import crystal.react.implicits._
import eu.timepit.refined.cats._
import eu.timepit.refined.types.numeric.PosLong
import eu.timepit.refined.types.string.NonEmptyString
import explore.AppCtx
import explore.GraphQLSchemas.ObservationDB
import explore.GraphQLSchemas.ObservationDB.Types._
import explore.Icons
import explore.common.SimbadSearch
import explore.common.TargetQueries
import explore.common.TargetQueries._
import explore.components.InputModal
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.components.undo.UndoRegion
import explore.data.KeyedIndexedList
import explore.implicits._
import explore.model.ExpandedIds
import explore.model.Focused
import explore.model.Focused._
import explore.model.ObsSummary
import explore.model.PointingId
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
import react.reflex._
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.Button.ButtonProps
import react.semanticui.elements.header.Header
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
  aimsWithObs: View[TargetsAndAsterismsWithObs],
  focused:     View[Option[Focused]],
  expandedIds: View[ExpandedIds],
  searching:   View[Set[Target.Id]]
) extends ReactProps[TargetObsList](TargetObsList.component)
    with ViewCommon {
  override def obsBadgeLayout = ObsBadge.Layout.ConfAndConstraints
}

object TargetObsList {
  type Props = TargetObsList

  @Lenses
  case class State(dragging: Boolean = false)

  val obsListMod      = new KIListMod[IO, ObsSummary, Observation.Id](ObsSummary.id)
  val targetListMod   = new KIListMod[IO, TargetIdName, Target.Id](TargetIdName.id)
  val asterismListMod = new KIListMod[IO, AsterismIdName, Asterism.Id](AsterismIdName.id)

  class Backend($ : BackendScope[Props, State]) {
    private val UnassignedObsId = "unassignedObs"

    def moveObs(obsId: Observation.Id, to: Option[PointingId])(implicit
      c:               TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      (to match {
        case Some(Right(targetId))  => AssignTargetToObs.execute(targetId, obsId)
        case Some(Left(asterismId)) => AssignAsterismToObs.execute(asterismId, obsId)
        case None                   => UnassignObs.execute(obsId)
      }).void

    def updateObs(input: EditObservationInput)(implicit
      c:                 TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      UpdateObservationMutation.execute(input).void

    def insertTarget(target: TargetIdName)(implicit
      c:                     TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      AddTarget
        .execute(target.id, target.name)
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
        .execute(asterism.id, asterism.name)
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

    private def getAimForObsWithId(
      obsWithIndexGetter: Getter[ObsList, obsListMod.ElemWithIndex]
    ): Getter[TargetsAndAsterismsWithObs, Option[Option[PointingId]]] =
      TargetsAndAsterismsWithObs.observations
        .composeGetter(
          obsWithIndexGetter
            .composeOptionLens(first)
            .composeOptionLens(ObsSummary.pointingId)
        )

    private def setAimForObsWithId(
      aimsWithObs:           View[TargetsAndAsterismsWithObs],
      obsId:                 Observation.Id,
      obsWithIndexGetAdjust: GetAdjust[ObsList, obsListMod.ElemWithIndex]
    )(implicit
      c:                     TransactionalClient[IO, ObservationDB]
    ): Option[Option[PointingId]] => IO[Unit] =
      aimOpt => {
        val obsAimAdjuster = obsWithIndexGetAdjust
          .composeOptionLens(first) // Focus on Observation within ElemWithIndex
          .composeOptionLens(ObsSummary.pointingId)

        val observationsView = aimsWithObs
          .zoom(TargetsAndAsterismsWithObs.observations)

        // 1) Update internal model
        observationsView.mod(obsAimAdjuster.set(aimOpt)) >>
          // 2) Send mutation
          aimOpt
            .map(newPointingId => moveObs(obsId, newPointingId))
            .orEmpty
      }

    protected def onDragEnd(
      setter:      Undoer.Setter[IO, TargetsAndAsterismsWithObs],
      expandedIds: View[ExpandedIds]
    )(implicit
      c:           TransactionalClient[IO, ObservationDB]
    ): (DropResult, ResponderProvided) => IO[Unit] =
      (result, _) =>
        $.propsIn[IO] >>= { props =>
          // println(scalajs.js.JSON.stringify(result))
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

                  // TODO Here we should flatten.
                  val set: Option[Option[PointingId]] => IO[Unit] =
                    setter
                      .set[Option[Option[PointingId]]](
                        props.aimsWithObs.get,
                        getAimForObsWithId(obsWithId.getter).get,
                        setAimForObsWithId(props.aimsWithObs, obsId, obsWithId)
                      )

                  destination.droppableId match {
                    case UnassignedObsId            =>
                      set(none.some)
                    case Target.Id(newTargetId)     =>
                      expandedIds.zoom(ExpandedIds.targetIds).mod(_ + newTargetId) >>
                        set(newTargetId.asRight.some.some)
                    case Asterism.Id(newAsterismId) =>
                      expandedIds
                        .zoom(ExpandedIds.asterismIds)
                        .mod(_ + newAsterismId) >>
                        set(newAsterismId.asLeft.some.some)

                    case _ => IO.unit // Report error?
                  }
                case None        =>
                  Target.Id.parse(result.draggableId) match {
                    case Some(targetId) =>
                      // Target dragged to asterism.
                      Asterism.Id.parse(destination.droppableId) match {
                        case Some(asterismId) =>
                          props.aimsWithObs.get.targets
                            .getElement(targetId)
                            .foldMap(target =>
                              expandedIds
                                .zoom(ExpandedIds.asterismIds)
                                .mod(_ + asterismId) >>
                                addTargetToAsterism(props.aimsWithObs, target, asterismId, setter)
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
      aimsWithObs:           View[TargetsAndAsterismsWithObs],
      focused:               View[Option[Focused]],
      targetId:              Target.Id,
      targetWithIndexSetter: Adjuster[TargetList, targetListMod.ElemWithIndex],
      nextToFocus:           Option[TargetIdName]
    )(implicit
      c:                     TransactionalClient[IO, ObservationDB]
    ): targetListMod.ElemWithIndex => IO[Unit] =
      targetWithIndex =>
        // 1) Update internal model
        aimsWithObs
          .zoom(TargetsAndAsterismsWithObs.targets)
          .mod(targetWithIndexSetter.set(targetWithIndex)) >>
          // 2) Send mutation & adjust focus
          targetWithIndex.fold(
            focused.set(nextToFocus.map(f => Focused.FocusedTarget(f.id))) >> removeTarget(targetId)
          ) { case (target, _) =>
            insertTarget(target) >> focused.set(FocusedTarget(targetId).some)
          }

    private def targetMod(
      setter:        Undoer.Setter[IO, TargetsAndAsterismsWithObs],
      aimsWithObs:   View[TargetsAndAsterismsWithObs],
      focused:       View[Option[Focused]],
      targetId:      Target.Id,
      focusOnDelete: Option[TargetIdName]
    )(implicit
      c:             TransactionalClient[IO, ObservationDB]
    ): targetListMod.Operation => IO[Unit] = {
      val targetWithId: GetAdjust[TargetList, targetListMod.ElemWithIndex] =
        targetListMod.withKey(targetId)

      setter
        .mod[targetListMod.ElemWithIndex](
          aimsWithObs.get,
          TargetsAndAsterismsWithObs.targets
            .composeGetter(targetWithId.getter)
            .get,
          setTargetWithIndex(aimsWithObs, focused, targetId, targetWithId.adjuster, focusOnDelete)
        )
    }

    protected def newTarget(
      setter: Undoer.Setter[IO, TargetsAndAsterismsWithObs]
    )(name:   NonEmptyString)(implicit
      c:      TransactionalClient[IO, ObservationDB],
      cs:     ContextShift[IO]
    ): IO[Unit] =
      ($.propsIn[IO], IO(PosLong.unsafeFrom(Random.nextInt().abs.toLong + 1))).parTupled.flatMap {
        case (props, posLong) =>
          val newTarget = TargetIdName(Target.Id(posLong), name)
          val mod       = targetMod(setter, props.aimsWithObs, props.focused, newTarget.id, none)
          (
            mod(targetListMod.upsert(newTarget, props.aimsWithObs.get.targets.length)),
            props.searching.mod(_ + newTarget.id) >>
              SimbadSearch
                .search(name)
                .attempt
                .guarantee(props.searching.mod(_ - newTarget.id))
          ).parTupled.flatMap {
            case (_, Right(Some(Target(_, Right(st), m)))) =>
              val update = TargetQueries.UpdateSiderealTracking(st) >>>
                TargetQueries.updateMagnitudes(m.values.toList)
              TargetMutation.execute(update(EditSiderealInput(newTarget.id))).void
            case _                                         =>
              IO.unit
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
        val mod = targetMod(setter, props.aimsWithObs, props.focused, targetId, focusOnDelete)
        mod(targetListMod.delete)
      }

    protected def newAsterism(
      setter: Undoer.Setter[IO, TargetsAndAsterismsWithObs]
    )(name:   NonEmptyString)(implicit
      c:      TransactionalClient[IO, ObservationDB]
    ): IO[Unit] = {
      // Temporary measure until we have id pools.
      val newAsterism = IO(Random.nextInt()).map(int =>
        AsterismIdName(Asterism.Id(PosLong.unsafeFrom(int.abs.toLong + 1)),
                       name,
                       KeyedIndexedList.empty
        )
      )

      $.propsIn[IO] >>= { props =>
        newAsterism >>= { asterism =>
          val mod = asterismMod(setter, props.aimsWithObs, props.focused, asterism.id, none)
          mod(asterismListMod.upsert(asterism, props.aimsWithObs.get.asterisms.length))
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
          asterismMod(setter, props.aimsWithObs, props.focused, asterismId, focusOnDelete)
        mod(asterismListMod.delete)
      }

    private def setAsterismWithIndex(
      aimsWithObs:             View[TargetsAndAsterismsWithObs],
      focused:                 View[Option[Focused]],
      asterismId:              Asterism.Id,
      asterismWithIndexSetter: Adjuster[AsterismList, asterismListMod.ElemWithIndex],
      nextToFoucs:             Option[AsterismIdName]
    )(implicit
      c:                       TransactionalClient[IO, ObservationDB]
    ): asterismListMod.ElemWithIndex => IO[Unit] = { asterismWithIndex =>
      val view = aimsWithObs
        .zoom(TargetsAndAsterismsWithObs.asterisms)

      // 1) Update internal model
      view
        .mod(asterismWithIndexSetter.set(asterismWithIndex)) >>
        // 2) Send mutation & adjust focus
        asterismWithIndex.fold(
          focused.set(nextToFoucs.map(f => FocusedAsterism(f.id))) >> removeAsterism(asterismId)
        ) { case (asterism, _) =>
          insertAsterism(asterism) >> focused.set(FocusedAsterism(asterismId).some)
        }
    }

    private def asterismMod(
      setter:        Undoer.Setter[IO, TargetsAndAsterismsWithObs],
      aimsWithObs:   View[TargetsAndAsterismsWithObs],
      focused:       View[Option[Focused]],
      asterismId:    Asterism.Id,
      focusOnDelete: Option[AsterismIdName]
    )(implicit
      c:             TransactionalClient[IO, ObservationDB]
    ): asterismListMod.Operation => IO[Unit] = {
      val asterismWithId: GetAdjust[AsterismList, asterismListMod.ElemWithIndex] =
        asterismListMod.withKey(asterismId)

      setter
        .mod[asterismListMod.ElemWithIndex](
          aimsWithObs.get,
          TargetsAndAsterismsWithObs.asterisms
            .composeGetter(asterismWithId.getter)
            .get,
          setAsterismWithIndex(aimsWithObs,
                               focused,
                               asterismId,
                               asterismWithId.adjuster,
                               focusOnDelete
          )
        )
    }

    private def setAsterismTargetWithIndex(
      aimsWithObs:           View[TargetsAndAsterismsWithObs],
      targetId:              Target.Id,
      asterismId:            Asterism.Id,
      targetWithIndexSetter: Adjuster[TargetList, targetListMod.ElemWithIndex]
    )(implicit
      c:                     TransactionalClient[IO, ObservationDB]
    ): targetListMod.ElemWithIndex => IO[Unit] =
      targetWithIndex =>
        // 1) Update internal model
        aimsWithObs
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
      setter:      Undoer.Setter[IO, TargetsAndAsterismsWithObs],
      aimsWithObs: View[TargetsAndAsterismsWithObs],
      targetId:    Target.Id,
      asterismId:  Asterism.Id
    )(implicit
      c:           TransactionalClient[IO, ObservationDB]
    ): targetListMod.Operation => IO[Unit] = {
      val targetWithId: GetAdjust[TargetList, targetListMod.ElemWithIndex] =
        targetListMod.withKey(targetId)

      setter
        .mod[targetListMod.ElemWithIndex](
          aimsWithObs.get,
          TargetsAndAsterismsWithObs.asterisms
            .composeGetter(asterismListMod.withKey(asterismId).getter)
            .map(_.map(_._1.targets).map(targetWithId.getter.get).flatten)
            .get,
          setAsterismTargetWithIndex(aimsWithObs, targetId, asterismId, targetWithId.adjuster)
        )
    }

    protected def addTargetToAsterism(
      aimsWithObs: View[TargetsAndAsterismsWithObs],
      target:      TargetIdName,
      asterismId:  Asterism.Id,
      setter:      Undoer.Setter[IO, TargetsAndAsterismsWithObs]
    )(implicit
      c:           TransactionalClient[IO, ObservationDB]
    ): IO[Unit] = {
      val mod = asterismTargetMod(setter, aimsWithObs, target.id, asterismId)
      mod(
        targetListMod.upsert(
          target,
          aimsWithObs.get.asterisms.getElement(asterismId).foldMap(_.targets.length)
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
        val mod = asterismTargetMod(setter, props.aimsWithObs, targetId, asterismId)
        mod(targetListMod.delete)
      }

    def toggleExpanded[A: Eq](
      id:          A,
      expandedIds: View[SortedSet[A]]
    ): IO[Unit] =
      expandedIds.mod { expanded =>
        expanded
          .exists(_ === id)
          .fold(expanded - id, expanded + id)
      }

    def render(props: Props, state: State): VdomElement = AppCtx.runWithCtx { implicit ctx =>
      val observations = props.aimsWithObs.get.observations
      val obsByAim     = observations.toList.groupBy(_.pointingId)

      val targets        = props.aimsWithObs.get.targets
      val targetsWithIdx = targets.toList.zipWithIndex

      val asterisms    = props.aimsWithObs.get.asterisms.toList
      val asterismIds  = asterisms.map(_.id)
      val asterismIdxs = asterisms.zipWithIndex

      val unassignedObs = obsByAim.get(none).orEmpty

      val renderClone: Draggable.Render =
        (provided, snapshot, rubric) => {
          <.div(provided.innerRef,
                provided.draggableProps,
                provided.dragHandleProps,
                props.getDraggedStyle(provided.draggableStyle, snapshot)
          )(
            (Target.Id
              .parse(rubric.draggableId)
              .toRight(Observation.Id.parse(rubric.draggableId)) match {
              case Right(targetId)   =>
                targets
                  .getElement(targetId)
                  .map(target => Card(raised = true)(CardContent(target.name.value)).vdomElement)
              case Left(Some(obsId)) => observations.getElement(obsId).map(props.renderObsBadge)
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
                InputModal(
                  "Create new Target",
                  initialValue = None,
                  label = "Name",
                  placeholder = "Target name",
                  okLabel = "Create",
                  onComplete = s => newTarget(undoCtx.setter)(s).runAsyncCB, // TODO Set coordinates
                  trigger = Button(size = Mini, compact = true)(
                    Icons.New.size(Small).fitted(true),
                    " Target"
                  )
                ),
                InputModal(
                  "Create new Asterism",
                  initialValue = None,
                  label = "Name",
                  placeholder = "Asterism name",
                  okLabel = "Create",
                  onComplete = s => newAsterism(undoCtx.setter)(s).runAsyncCB,
                  trigger = Button(size = Mini, compact = true)(
                    Icons.New.size(Small).fitted(true),
                    " Asterism"
                  )
                )
              ),
              UndoButtons(props.aimsWithObs.get, undoCtx, size = Mini)
            ),
            ReflexContainer()(
              List[Option[VdomNode]](
                // Start Target Tree
                (ReflexElement(minSize = 36, clazz = ExploreStyles.ObsTreeSection)(
                  Header(block = true, clazz = ExploreStyles.ObsTreeHeader)("TARGETS"),
                  <.div(ExploreStyles.ObsTree)(
                    <.div(ExploreStyles.ObsScrollTree)(
                      targetsWithIdx.toTagMod { case (target, targetIdx) =>
                        val targetId      = target.id
                        val nextToSelect  = targetsWithIdx.find(_._2 === targetIdx + 1).map(_._1)
                        val prevToSelect  = targetsWithIdx.find(_._2 === targetIdx - 1).map(_._1)
                        val focusOnDelete = nextToSelect.orElse(prevToSelect)

                        val targetObs = obsByAim.get(targetId.asRight.some).orEmpty

                        val expandedTargetIds =
                          props.expandedIds.zoom(ExpandedIds.targetIds)
                        val opIcon            =
                          targetObs.nonEmpty.fold(
                            Icon(
                              "chevron " + expandedTargetIds.get
                                .exists(_ === targetId)
                                .fold("down", "right")
                            )(^.cursor.pointer,
                              ^.onClick ==> { e: ReactEvent =>
                                e.stopPropagationCB >>
                                  toggleExpanded(targetId, expandedTargetIds).runAsyncCB
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
                                    e.stopPropagationCB >>
                                      deleteTarget(targetId,
                                                   undoCtx.setter,
                                                   focusOnDelete
                                      ).runAsyncCB
                                )(
                                  Icons.Trash
                                ),
                                <.span(ExploreStyles.ObsCount, s"${targetObs.length} Obs")
                              )

                            <.div(
                              provided.innerRef,
                              provided.droppableProps,
                              props.getListStyle(
                                snapshot.draggingOverWith.exists(id =>
                                  Observation.Id.parse(id).isDefined
                                )
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
                                // Drag without removing from list.
                                if (asterisms.isEmpty || shouldRenderClone)
                                  targetHeader
                                else
                                  Draggable(targetId.toString, -1) {
                                    case (targetProvided, targetSnapshot, _) =>
                                      <.span(
                                        targetProvided.innerRef,
                                        targetProvided.draggableProps,
                                        props.getDraggedStyle(targetProvided.draggableStyle,
                                                              targetSnapshot
                                        ),
                                        targetProvided.dragHandleProps
                                      )(
                                        targetHeader
                                      )
                                  },
                                TagMod
                                  .when(expandedTargetIds.get.contains(targetId))(
                                    targetObs.zipWithIndex.toTagMod(
                                      (props.renderObsBadgeItem(selectable = true) _).tupled
                                    )
                                  ),
                                provided.placeholder
                              )
                            )
                        }
                      }
                    )
                  )
                ): VdomNode).some,
                (ReflexSplitter(propagate = true): VdomNode).some.filter(_ => asterisms.nonEmpty),
                (ReflexElement(minSize = 36, clazz = ExploreStyles.ObsTreeSection)(
                  ReflexHandle()(
                    Header(block = true, clazz = ExploreStyles.ObsTreeHeader)("ASTERISMS")
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
                        val asterismObs     = obsByAim.get(asterismId.asLeft.some).orEmpty

                        val expandedAsterismIds =
                          props.expandedIds.zoom(ExpandedIds.asterismIds)
                        val opIcon              =
                          (asterismObs.nonEmpty || asterismTargets.nonEmpty).fold(
                            Icon(
                              "chevron " + expandedAsterismIds.get
                                .exists(_ === asterismId)
                                .fold("down", "right")
                            )(^.cursor.pointer,
                              ^.onClick ==> { e: ReactEvent =>
                                e.stopPropagationCB >>
                                  toggleExpanded(asterismId, expandedAsterismIds).runAsyncCB
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
                            props.getListStyle(snapshot.isDraggingOver)
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
                                      deleteAsterism(asterismId,
                                                     undoCtx.setter,
                                                     focusOnDelete
                                      ).runAsyncCB
                                )(
                                  Icons.Trash
                                ),
                                <.span(
                                  ExploreStyles.ObsCount,
                                  s"${asterismTargets.length} Tgts - ${asterismObs.length} Obs"
                                )
                              ),
                              TagMod.when(expandedAsterismIds.get.contains(asterismId))(
                                <.div(ExploreStyles.ObsTreeItem)(
                                  SegmentGroup(
                                    asterismTargets
                                      .sortBy(_.name)
                                      .toTagMod(target =>
                                        Segment(basic = true,
                                                clazz = ExploreStyles.ObsTreeGroupHeader
                                        )(
                                          <.span(ExploreStyles.TargetLabelTitle)(
                                            target.name.value
                                          ),
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
                                    (props.renderObsBadgeItem(selectable = true) _).tupled
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
                ): VdomNode).some.filter(_ => asterisms.nonEmpty),
                (ReflexSplitter(propagate = true): VdomNode).some,
                (ReflexElement(size = 36,
                               minSize = 36,
                               clazz = ExploreStyles.ObsTreeSection,
                               withHandle = true
                )(
                  ReflexWithHandle(reflexProvided =>
                    // End Asterism Tree - Start Unassigned Observations List
                    Droppable(UnassignedObsId) { case (provided, snapshot) =>
                      <.div(ExploreStyles.ObsUnassigned,
                            provided.innerRef,
                            provided.droppableProps,
                            props.getListStyle(snapshot.isDraggingOver)
                      )(
                        ReflexHandle(provided = reflexProvided)(
                          Header(
                            block = true,
                            clazz = ExploreStyles.ObsTreeHeader |+| ExploreStyles.ObsTreeGroupHeader
                          )(
                            <.span(ExploreStyles.TargetLabelTitle)("UNASSIGNED OBSERVATIONS"),
                            <.span(ExploreStyles.ObsCount, s"${unassignedObs.length} Obs")
                          )
                        ),
                        <.div(ExploreStyles.ObsTree)(
                          <.div(ExploreStyles.ObsScrollTree) {

                            Segment(
                              vertical = true,
                              clazz = ExploreStyles.ObsTreeGroup
                            )(
                              unassignedObs.zipWithIndex.toTagMod(
                                (props.renderObsBadgeItem(selectable = false) _).tupled
                              ),
                              provided.placeholder
                            )
                          }
                        )
                      )
                    }
                  )
                ): VdomNode).some
                // End Unassigned Observations List
              ).flatten: _*
            )
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
        val aimsWithObs         = $.props.aimsWithObs.get
        val expandedTargetIds   = $.props.expandedIds.zoom(ExpandedIds.targetIds)
        val expandedAsterismIds = $.props.expandedIds.zoom(ExpandedIds.asterismIds)

        // Expand target or asterism with focused observation
        val expandObservationObject =
          $.props.focused.get
            .collect { case FocusedObs(obsId) =>
              aimsWithObs.observations
                .getElement(obsId)
                .flatMap(_.pointingId.map(_ match {
                  case Right(targetId)  => expandedTargetIds.mod(_ + targetId)
                  case Left(asterismId) => expandedAsterismIds.mod(_ + asterismId)
                }))
            }
            .flatten
            .orEmpty

        // Remove objects from expanded set which are no longer present.
        val removeTargets =
          (expandedTargetIds.get -- aimsWithObs.targets.toList
            .map(_.id)).toNes
            .map(removedTargetIds => expandedTargetIds.mod(_ -- removedTargetIds.toSortedSet))
            .orEmpty

        val removeAsterisms =
          (expandedAsterismIds.get -- aimsWithObs.asterisms.toList
            .map(_.id)).toNes
            .map(removedAsterismIds => expandedAsterismIds.mod(_ -- removedAsterismIds.toSortedSet))
            .orEmpty

        (expandObservationObject >> removeTargets >> removeAsterisms).runAsyncAndForgetCB
      }
      .build
}
