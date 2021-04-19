// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import cats.effect.IO
import cats.syntax.all._
import clue.TransactionalClient
import crystal.ViewF
import crystal.react.implicits._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.PosLong
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.ConstraintSetObsQueries
import explore.common.ConstraintSetObsQueries._
import explore.common.ConstraintSetObsQueriesGQL._
import explore.components.InputModal
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.components.undo.UndoRegion
import explore.implicits._
import explore.model.ConstraintsSummary
import explore.model.Focused
import explore.model.Focused._
import explore.model.ObsSummaryWithPointingAndConstraints
import explore.model.reusability._
import explore.optics.GetAdjust
import explore.optics._
import explore.schemas.ObservationDB
import explore.undo.KIListMod
import explore.undo.Undoer
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.ui.reusability._
import monocle.Getter
import monocle.function.Field1.first
import monocle.macros.Lenses
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
import react.semanticui.sizes._

import scala.collection.immutable.SortedSet
import scala.util.Random

final case class ConstraintSetObsList(
  constraintSetsWithObs: View[ConstraintSetsWithObs],
  focused:               View[Option[Focused]],
  expandedIds:           View[SortedSet[ConstraintSet.Id]]
)(implicit val ctx:      AppContextIO)
    extends ReactProps[ConstraintSetObsList](ConstraintSetObsList.component)
    with ViewCommon
object ConstraintSetObsList {
  type Props = ConstraintSetObsList

  @Lenses
  case class State(dragging: Boolean = false)

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive
  protected implicit val stateReuse: Reusability[State] = Reusability.derive

  val obsListMod           =
    new KIListMod[IO, ObsSummaryWithPointingAndConstraints, Observation.Id](
      ObsSummaryWithPointingAndConstraints.id
    )
  val constraintSetListMod =
    new KIListMod[IO, ConstraintsSummary, ConstraintSet.Id](ConstraintsSummary.id)

  class Backend($ : BackendScope[Props, State]) {
    private val UnassignedObsId = "unassignedObs"

    def moveObs(
      obsId:   Observation.Id,
      csIdOpt: Option[ConstraintSet.Id]
    )(implicit
      c:       TransactionalClient[IO, ObservationDB]
    ): IO[Unit] =
      csIdOpt match {
        case Some(csId) => AssignConstraintSetToObs.execute(csId, obsId).void
        case None       => UnassignConstraintSetFromObs.execute(obsId).void
      }

    def insertConstraintSet(cs: ConstraintsSummary)(implicit
      c:                        TransactionalClient[IO, ObservationDB]
    ): IO[Unit] = {
      val create = ConstraintSetObsQueries.defaultCreateConstraintSet(cs)
      AddConstraintSet
        .execute(create)
        .handleErrorWith(_ => UndeleteConstraintSet.execute(cs.id))
        .void
    }

    def deleteConstraintSet(id: ConstraintSet.Id)(implicit
      c:                        TransactionalClient[IO, ObservationDB]
    ): IO[Unit] = DeleteConstraintSet.execute(id).void

    private def getConstraintSetForObsWithId(
      obsWithIndexGetter: Getter[ObsList, obsListMod.ElemWithIndex]
    ): Getter[ConstraintSetsWithObs, Option[Option[ConstraintsSummary]]] =
      ConstraintSetsWithObs.obs.composeGetter(
        obsWithIndexGetter
          .composeOptionLens(first)
          .composeOptionLens(ObsSummaryWithPointingAndConstraints.constraints)
      )

    private def setConstraintSetForObsWithId(
      constraintSetsWithObs: View[ConstraintSetsWithObs],
      obsId:                 Observation.Id,
      obsWithIndexGetAdjust: GetAdjust[ObsList, obsListMod.ElemWithIndex]
    )(implicit
      c:                     TransactionalClient[IO, ObservationDB]
    ): Option[Option[ConstraintsSummary]] => IO[Unit] = { csOpt =>
      val obsCsAdjuster = obsWithIndexGetAdjust
        .composeOptionLens(first)
        .composeOptionLens(ObsSummaryWithPointingAndConstraints.constraints)

      val observationsView = constraintSetsWithObs.zoom(ConstraintSetsWithObs.obs)

      // 1) Update internal model
      observationsView.mod(obsCsAdjuster.set(csOpt)) >>
        // 2) Send mutation
        csOpt.map(newCs => moveObs(obsId, newCs.map(_.id))).orEmpty

    }

    protected def onDragEnd(
      setter:      Undoer.Setter[IO, ConstraintSetsWithObs],
      expandedIds: View[SortedSet[ConstraintSet.Id]]
    )(implicit
      c:           TransactionalClient[IO, ObservationDB]
    ): (DropResult, ResponderProvided) => IO[Unit] =
      (result, _) =>
        $.propsIn[IO] >>= { props =>
          result.destination.toOption
            .map(destination =>
              result.draggableId match {
                case Observation.Id(obsId) =>
                  val obsWithId: GetAdjust[ObsList, obsListMod.ElemWithIndex] =
                    obsListMod.withKey(obsId)

                  val set: Option[Option[ConstraintsSummary]] => IO[Unit] =
                    setter.set[Option[Option[ConstraintsSummary]]](
                      props.constraintSetsWithObs.get,
                      getConstraintSetForObsWithId(obsWithId.getter).get,
                      setConstraintSetForObsWithId(props.constraintSetsWithObs, obsId, obsWithId)
                    )

                  def getSummary(csId: ConstraintSet.Id): IO[ConstraintsSummary] = {
                    val csEither: Either[Throwable, ConstraintsSummary] = constraintSetListMod
                      .getterForKey(csId)
                      .get(props.constraintSetsWithObs.get.constraintSets)
                      .map(_._1)
                      .toRight(new Exception("Not found"))
                    IO.fromEither(csEither)
                  }

                  destination.droppableId match {
                    case UnassignedObsId           => set(none.some)
                    case ConstraintSet.Id(newCsId) =>
                      getSummary(newCsId).flatMap(cs =>
                        expandedIds.mod(_ + newCsId) >>
                          set(cs.some.some)
                      )
                    case _                         => IO.unit
                  }
                case _                     => IO.unit
              }
            )
            .orEmpty
        }

    private def setConstraintSetWithIndex(
      constraintSetsWithObs: View[ConstraintSetsWithObs],
      focused:               View[Option[Focused]],
      csId:                  ConstraintSet.Id,
      csWithIndexSetter:     Adjuster[ConstraintSetList, constraintSetListMod.ElemWithIndex],
      nextToFocus:           Option[ConstraintsSummary]
    )(implicit
      c:                     TransactionalClient[IO, ObservationDB]
    ): constraintSetListMod.ElemWithIndex => IO[Unit] =
      csWithIndex =>
        constraintSetsWithObs
          .zoom(ConstraintSetsWithObs.constraintSets)
          .mod(csWithIndexSetter.set(csWithIndex)) >>
          csWithIndex.fold(
            focused.set(
              nextToFocus.map(cs => Focused.FocusedConstraintSet(cs.id))
            ) >> deleteConstraintSet(csId)
          ) { case (cs, _) =>
            insertConstraintSet(cs) >> focused.set(FocusedConstraintSet(csId).some)
          }

    private def constraintSetMod(
      setter:                Undoer.Setter[IO, ConstraintSetsWithObs],
      constraintSetsWithObs: View[ConstraintSetsWithObs],
      focused:               View[Option[Focused]],
      csId:                  ConstraintSet.Id,
      focusOnDelete:         Option[ConstraintsSummary]
    )(implicit
      c:                     TransactionalClient[IO, ObservationDB]
    ): constraintSetListMod.Operation => IO[Unit] = {
      val csWithId: GetAdjust[ConstraintSetList, constraintSetListMod.ElemWithIndex] =
        constraintSetListMod.withKey(csId)

      setter.mod[constraintSetListMod.ElemWithIndex](
        constraintSetsWithObs.get,
        ConstraintSetsWithObs.constraintSets.composeGetter(csWithId.getter).get,
        setConstraintSetWithIndex(constraintSetsWithObs,
                                  focused,
                                  csId,
                                  csWithId.adjuster,
                                  focusOnDelete
        )
      )
    }

    def newConstraintSet(
      setter: Undoer.Setter[IO, ConstraintSetsWithObs]
    )(name:   NonEmptyString)(implicit c: TransactionalClient[IO, ObservationDB]): IO[Unit] = {
      // Temporary measure until we have id pools.
      val newCs = IO(Random.nextInt(0xfff)).map(int =>
        ConstraintsSummary.default(id = ConstraintSet.Id(PosLong.unsafeFrom(int.abs.toLong + 1)),
                                   name = name
        )
      )

      $.propsIn[IO] >>= { props =>
        newCs >>= { cs =>
          val mod =
            constraintSetMod(setter, props.constraintSetsWithObs, props.focused, cs.id, none)
          mod(
            constraintSetListMod.upsert(cs, props.constraintSetsWithObs.get.constraintSets.length)
          )
        }
      }
    }

    def deleteConstraintSet(
      csId:          ConstraintSet.Id,
      setter:        Undoer.Setter[IO, ConstraintSetsWithObs],
      focusOnDelete: Option[ConstraintsSummary]
    )(implicit c:    TransactionalClient[IO, ObservationDB]): IO[Unit] =
      $.propsIn[IO] >>= { props =>
        val mod =
          constraintSetMod(setter, props.constraintSetsWithObs, props.focused, csId, focusOnDelete)
        mod(constraintSetListMod.delete)
      }

    def toggleExpanded(
      id:          ConstraintSet.Id,
      expandedIds: View[SortedSet[ConstraintSet.Id]]
    ): IO[Unit] =
      expandedIds
        .mod(expanded => expanded.exists(_ === id).fold(expanded - id, expanded + id))

    def renderFn(
      props:   Props,
      state:   View[State],
      undoCtx: Undoer.Context[IO, ConstraintSetsWithObs]
    ): VdomNode = {
      implicit val ctx = props.ctx

      val observations       = props.constraintSetsWithObs.get.obs
      val obsByConstraintSet = observations.toList.groupBy(_.constraints.map(_.id))

      val constraintSets        = props.constraintSetsWithObs.get.constraintSets
      val constraintSetsWithIdx = constraintSets.toList.zipWithIndex

      val unassignedObs = obsByConstraintSet.get(none).orEmpty

      val renderClone: Draggable.Render =
        (provided, snapshot, rubric) => {
          <.div(provided.innerRef,
                provided.draggableProps,
                provided.dragHandleProps,
                props.getDraggedStyle(provided.draggableStyle, snapshot)
          )(
            (rubric.draggableId match {
              case Observation.Id(obsId) =>
                observations.getElement(obsId).map(props.renderObsBadge)
              case _                     => none
            }).getOrElse(<.span("ERROR"))
          )
        }

      val handleDragEnd = onDragEnd(undoCtx.setter, props.expandedIds)

      def createConstraintSet(name: NonEmptyString): Callback =
        newConstraintSet(undoCtx.setter)(name).runAsyncCB

      DragDropContext(
        onDragStart =
          (_: DragStart, _: ResponderProvided) => state.zoom(State.dragging).set(true).runAsyncCB,
        onDragEnd = (result, provided) =>
          (state.zoom(State.dragging).set(false) >> handleDragEnd(result, provided)).runAsyncCB
      )(
        <.div(ExploreStyles.ObsTreeWrapper)(
          <.div(ExploreStyles.TreeToolbar)(
            InputModal(
              "Create new Constraint",
              initialValue = None,
              label = "Name",
              placeholder = "Constraint name",
              okLabel = "Create",
              onComplete = (createConstraintSet _).reusable,
              trigger = Button(size = Mini, compact = true)(
                Icons.New.size(Small).fitted(true)
              )
            ),
            UndoButtons(props.constraintSetsWithObs.get, undoCtx, size = Mini)
          ),
          ReflexContainer()(
            // Start constraint sets tree
            ReflexElement(minSize = 36, clazz = ExploreStyles.ObsTreeSection)(
              Header(block = true, clazz = ExploreStyles.ObsTreeHeader)("Constraints"),
              <.div(ExploreStyles.ObsTree)(
                <.div(ExploreStyles.ObsScrollTree)(
                  constraintSetsWithIdx.toTagMod { case (constraintSet, csIdx) =>
                    val csId          = constraintSet.id
                    val nextToSelect  = constraintSetsWithIdx.find(_._2 === csIdx + 1).map(_._1)
                    val prevToSelect  = constraintSetsWithIdx.find(_._2 === csIdx - 1).map(_._1)
                    val focusOnDelete = nextToSelect.orElse(prevToSelect)

                    val csObs = obsByConstraintSet.get(csId.some).orEmpty

                    val opIcon = csObs.nonEmpty.fold(
                      Icon(
                        "chevron " + props.expandedIds.get
                          .exists(_ === csId)
                          .fold("down", "right")
                      )(^.cursor.pointer,
                        ^.onClick ==> { e: ReactEvent =>
                          e.stopPropagationCB >>
                            toggleExpanded(csId, props.expandedIds).runAsyncCB
                              .asEventDefault(e)
                              .void
                        }
                      ),
                      Icons.ChevronRight
                    )

                    val obsSelected = props.focused.get
                      .exists(f => csObs.map(obs => FocusedObs(obs.id)).exists(f === _))

                    Droppable(csId.toString, renderClone = renderClone) {
                      case (provided, snapshot) =>
                        val csHeader = <.span(ExploreStyles.ObsTreeGroupHeader)(
                          <.span(ExploreStyles.ObsGroupTitle)(
                            opIcon,
                            constraintSet.name.value
                          ),
                          Button(
                            size = Small,
                            compact = true,
                            clazz = ExploreStyles.DeleteButton |+| ExploreStyles.JustifyRight,
                            onClickE = (e: ReactMouseEvent, _: ButtonProps) =>
                              e.stopPropagationCB >>
                                deleteConstraintSet(csId, undoCtx.setter, focusOnDelete).runAsyncCB
                          )(
                            Icons.Trash
                          ),
                          <.span(ExploreStyles.ObsCount, s"${csObs.length} Obs")
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
                                  obsSelected || props.focused.get
                                    .exists(_ === FocusedConstraintSet(csId))
                                )(ExploreStyles.SelectedObsTreeGroup)
                                .orElse(
                                  Option
                                    .when(!state.get.dragging)(
                                      ExploreStyles.UnselectedObsTreeGroup
                                    )
                                )
                                .orEmpty
                          )(
                            ^.cursor.pointer,
                            ^.onClick --> props.focused
                              .set(FocusedConstraintSet(csId).some)
                              .runAsyncCB
                          )(
                            csHeader,
                            TagMod.when(props.expandedIds.get.contains(csId))(
                              csObs.zipWithIndex.toTagMod(
                                (props.renderObsBadgeItem(selectable = true) _).tupled
                              )
                            ),
                            <.span(provided.placeholder)
                          )
                        )
                    }
                  }
                )
              )
            ),
            // end of constraint set tree
            ReflexSplitter(propagate = true),
            // start of unassigned observations list
            ReflexElement(size = 36,
                          minSize = 36,
                          clazz = ExploreStyles.ObsTreeSection,
                          withHandle = true
            )(
              ReflexWithHandle(reflexProvided =>
                Droppable(UnassignedObsId) { case (provided, snapshot) =>
                  <.div(
                    ExploreStyles.ObsUnassigned,
                    provided.innerRef,
                    provided.droppableProps,
                    props.getListStyle(snapshot.isDraggingOver)
                  )(
                    ReflexHandle(provided = reflexProvided)(
                      Header(block = true,
                             clazz =
                               ExploreStyles.ObsTreeHeader |+| ExploreStyles.ObsTreeGroupHeader
                      )(
                        <.span(ExploreStyles.ObsGroupTitle)("Unassigned observations"),
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
            )
            // end of unassigned observations list
          )
        )
      )
    }

    def render(props: Props) = {
      implicit val ctx = props.ctx
      UndoRegion[ConstraintSetsWithObs]((renderFn _).reusable(props, ViewF.fromState[IO]($)))
    }
  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .initialState(State())
      .renderBackend[Backend]
      .componentDidMount { $ =>
        implicit val ctx          = $.props.ctx
        val constraintSetsWithObs = $.props.constraintSetsWithObs.get
        val expandedIds           = $.props.expandedIds

        // expand constraint set with focused observation
        val expandCs = $.props.focused.get
          .collect { case FocusedObs(obsId) =>
            constraintSetsWithObs.obs
              .getElement(obsId)
              .flatMap(_.constraints.map(c => expandedIds.mod(_ + c.id)))
          }
          .flatten
          .orEmpty

        // Remove contraint sets from expanded list which no longer exist.
        val removeConstraintSets =
          (expandedIds.get -- constraintSetsWithObs.constraintSets.toList.map(_.id)).toNes
            .map(missingIds => expandedIds.mod(_ -- missingIds.toSortedSet))
            .orEmpty

        (expandCs >> removeConstraintSets).runAsyncCB
      }
      .configure(Reusability.shouldComponentUpdate)
      .build
}
