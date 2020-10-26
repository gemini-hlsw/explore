// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import scala.annotation.unused
import scala.collection.immutable.HashSet
import scala.util.Random

import cats.effect.IO
import cats.syntax.all._
import crystal.react.implicits._
import eu.timepit.refined.types.numeric.PosLong
import explore.Icons
import explore.components.ObsBadge
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.components.undo.UndoRegion
import explore.implicits._
import explore.model.Focused
import explore.model.Focused._
import explore.model.ObsSummary
import explore.optics.GetAdjust
import explore.optics._
import explore.undo.KIListMod
import explore.undo.Undoer
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.model.Target
import monocle.Getter
import monocle.function.Field1.first
import monocle.macros.Lenses
import mouse.boolean._
import react.beautifuldnd._
import react.common._
import react.semanticui.elements.button.Button
import react.semanticui.elements.icon.Icon
import react.semanticui.elements.segment.Segment
import react.semanticui.sizes._

import TargetObsQueries._

final case class TargetObsList(
  targetsWithObs: View[TargetsWithObs],
  focused:        View[Option[Focused]]
) extends ReactProps[TargetObsList](TargetObsList.component)

object TargetObsList {
  type Props = TargetObsList

  @Lenses
  case class State(collapsedTargetIds: Set[Target.Id] = HashSet.empty)

  val obsListMod    =
    new KIListMod[IO, ObsIdNameTarget, Observation.Id](ObsIdNameTarget.id)
  val targetListMod = new KIListMod[IO, TargetIdName, Target.Id](TargetIdName.id)

  class Backend($ : BackendScope[Props, State]) {

    private def getTargetForObsWithId(
      obsWithIndexGetter: Getter[ObsList, obsListMod.ElemWithIndex]
    ): Getter[TargetsWithObs, Option[TargetIdName]] =
      Getter { two =>
        val targetSummary =
          TargetsWithObs.obs
            .composeGetter(
              obsWithIndexGetter
                .composeOptionLens(first)
                .composeOptionLens(ObsIdNameTarget.target)
            )
            .get(two)
        targetSummary.flatMap(ts => two.targets.getElement(ts.id))
      }

    private def setTargetForObsWithId(
      targetsWithObs:     View[TargetsWithObs],
      @unused obsId:      Observation.Id,
      obsWithIndexSetter: Adjuster[ObsList, obsListMod.ElemWithIndex]
    ): Option[TargetIdName] => IO[Unit] =
      targetOpt =>
        // 1) Update internal model
        targetsWithObs
          .zoom(TargetsWithObs.obs)
          .mod(
            obsWithIndexSetter
              .composeOptionLens(first)
              .composeOptionLens(ObsIdNameTarget.target)
              .set(targetOpt) //.map(TargetSummary.fromTarget))
          ) //>>
    // 2) Send mutation
    // updateObs(EditObservationInput(obsId, targets = targetOpt.map(_.id)).some)

    protected def onDragEnd(
      setter: Undoer.Setter[IO, TargetsWithObs]
    ): (DropResult, ResponderProvided) => Callback =
      (result, _) =>
        $.props >>= { props =>
          (for {
            newTargetIdStr <- result.destination.toOption.map(_.droppableId)
            newTargetId    <- Target.Id.parse(newTargetIdStr)
            target         <- props.targetsWithObs.get.targets.getElement(newTargetId)
            obsId          <- Observation.Id.parse(result.draggableId)
          } yield {
            val obsWithId: GetAdjust[ObsList, obsListMod.ElemWithIndex] =
              obsListMod.withKey(obsId)

            val set: Option[TargetIdName] => IO[Unit] =
              setter
                .set[Option[TargetIdName]](
                  props.targetsWithObs.get,
                  getTargetForObsWithId(obsWithId.getter).get,
                  setTargetForObsWithId(props.targetsWithObs, obsId, obsWithId.adjuster)
                )

            set(target.some).runInCB
          }).getOrEmpty
        }

    private def setTargetWithIndex(
      targetsWithObs:        View[TargetsWithObs],
      focused:               View[Option[Focused]],
      targetId:              Target.Id,
      targetWithIndexSetter: Adjuster[TargetList, targetListMod.ElemWithIndex]
    ): targetListMod.ElemWithIndex => IO[Unit] =
      targetWithIndex =>
        // 1) Update internal model
        targetsWithObs
          .zoom(TargetsWithObs.targets)
          .mod(targetWithIndexSetter.set(targetWithIndex)) >>
          // 2) Send mutation & adjust focus
          targetWithIndex.fold(focused.set(none) >> removeTarget(targetId)) { case (target, _) =>
            insertTarget(target) >> focused.set(FocusedTarget(targetId).some)
          }

    private def targetMod(
      setter:         Undoer.Setter[IO, TargetsWithObs],
      targetsWithObs: View[TargetsWithObs],
      focused:        View[Option[Focused]],
      targetId:       Target.Id
    ): targetListMod.Operation => IO[Unit] = {
      val targetWithId: GetAdjust[TargetList, targetListMod.ElemWithIndex] =
        targetListMod.withKey(targetId)

      setter
        .mod[targetListMod.ElemWithIndex](
          targetsWithObs.get,
          TargetsWithObs.targets
            .composeGetter(targetWithId.getter)
            .get,
          setTargetWithIndex(targetsWithObs, focused, targetId, targetWithId.adjuster)
        )
    }

    protected def newTarget(setter: Undoer.Setter[IO, TargetsWithObs]): Callback =
      $.props >>= { props =>
        // Temporary measure until we have id pools.
        val newTarget =
          TargetIdName(Target.Id(PosLong.unsafeFrom(Random.nextLong().abs.toLong + 1)),
                       "<New Target>"
          )

        val upsert =
          targetListMod
            .upsert(newTarget, props.targetsWithObs.get.targets.length)

        targetMod(setter, props.targetsWithObs, props.focused, newTarget.id)(upsert).runInCB
      }

    protected def deleteTargetEnabled(
      focused:        Option[Focused],
      targetsWithObs: TargetsWithObs
    ): Option[Target.Id] =
      focused.collect {
        case FocusedTarget(targetId) if !targetsWithObs.obs.exists(_.target.id === targetId) =>
          targetId
      }

    protected def deleteTarget(setter: Undoer.Setter[IO, TargetsWithObs]): Callback =
      $.props >>= { props =>
        deleteTargetEnabled(props.focused.get, props.targetsWithObs.get)
          .map(targetId =>
            targetMod(setter, props.targetsWithObs, props.focused, targetId)(
              targetListMod.delete
            ).runInCB
          )
          .getOrEmpty
      }

    def toggleCollapsed(targetId: Target.Id): Callback =
      $.modStateL(State.collapsedTargetIds) { collapsed =>
        collapsed
          .exists(_ === targetId)
          .fold(collapsed - targetId, collapsed + targetId)
      }

    // Adapted from https://github.com/atlassian/react-beautiful-dnd/issues/374#issuecomment-569817782
    def getObsStyle(style:          TagMod, snapshot:    Draggable.StateSnapshot): TagMod =
      if (!snapshot.isDragging)
        TagMod.empty
      else if (!snapshot.isDropAnimating)
        style
      else
        TagMod(style, ^.transitionDuration := "0.001s")

    def decorateTopRight(decorated: VdomNode, decorator: VdomNode): VdomNode              =
      <.div(^.position.relative)(
        <.div(^.position.absolute,
              ^.top := "0",
              ^.right := "0",
              ^.zIndex := "10",
              ^.marginTop := "5px",
              decorator
        ),
        decorated
      )

    def getListStyle(isDragging: Boolean): TagMod =
      ExploreStyles.DraggingOver.when(isDragging)

    def render(props: Props, state: State): VdomElement = {
      val obsByTarget = props.targetsWithObs.get.obs.toList.groupBy(_.target.id)

      <.div(ExploreStyles.ObsTree)(
        UndoRegion[TargetsWithObs] { undoCtx =>
          DragDropContext(onDragEnd = onDragEnd(undoCtx.setter))(
            <.div(
              props.targetsWithObs.get.targets.elements.toTagMod { target =>
                val targetId = target.id

                val targetObs = obsByTarget.get(targetId).orEmpty
                val obsCount  = targetObs.length

                val opIcon =
                  targetObs.nonEmpty.fold(
                    Icon(
                      "chevron " + state.collapsedTargetIds
                        .exists(_ === targetId)
                        .fold("right", "down")
                    )(^.cursor.pointer,
                      ^.onClick ==> { e: ReactEvent =>
                        toggleCollapsed(targetId).asEventDefault(e).void
                      }
                    ),
                    Icons.ChevronRight
                  )

                Droppable(target.id.toString) { case (provided, snapshot) =>
                  <.div(
                    provided.innerRef,
                    provided.droppableProps,
                    getListStyle(snapshot.isDraggingOver)
                  )(
                    Segment(vertical = true,
                            raised = props.focused.get
                              .exists(_ === FocusedTarget(target.id)),
                            clazz = ExploreStyles.ObsTreeGroup
                    )(
                      ^.onClick --> props.focused.set(FocusedTarget(targetId).some).runInCB
                    )(
                      <.span(ExploreStyles.ObsTreeGroupHeader)(
                        <.span(
                          opIcon,
                          target.name
                        ),
                        <.span(^.float.right, s"$obsCount Obs")
                      ),
                      TagMod.when(!state.collapsedTargetIds.contains(targetId))(
                        targetObs.zipWithIndex.toTagMod { case (obs, idx) =>
                          <.div(ExploreStyles.ObsTreeItem)(
                            Draggable(obs.id.toString, idx) { case (provided, snapshot, _) =>
                              def dragIcon =
                                <.span(
                                  provided.dragHandleProps,
                                  Icons.Sort
                                )

                              <.div(
                                provided.innerRef,
                                provided.draggableProps,
                                getObsStyle(provided.draggableStyle, snapshot),
                                ^.onClick ==> { e: ReactEvent =>
                                  e.stopPropagationCB >>
                                    props.focused
                                      .set(FocusedObs(obs.id).some)
                                      .runInCB
                                }
                              )(
                                decorateTopRight(
                                  ObsBadge(ObsSummary(obs.id, obs.target.name),
                                           ObsBadge.Layout.ConfAndConstraints,
                                           selected =
                                             props.focused.get.exists(_ === FocusedObs(obs.id))
                                  ),
                                  dragIcon
                                )
                              )
                            }
                          )
                        }
                      ),
                      provided.placeholder
                      //<.span(^.display.none.when(targetObs.nonEmpty), provided.placeholder) // Doesn't really work.
                    )
                  )
                }
              }
            ),
            <.div(ExploreStyles.ObsTreeButtons)(
              <.div(
                Button(size = Small, onClick = newTarget(undoCtx.setter))(
                  Icons.New
                ),
                Button(size = Small,
                       onClick = deleteTarget(undoCtx.setter),
                       disabled =
                         deleteTargetEnabled(props.focused.get, props.targetsWithObs.get).isEmpty
                )(Icons.Delete)
              ),
              UndoButtons(props.targetsWithObs.get, undoCtx)
            )
          )
        }
      )
    }
  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .initialState(State())
      .renderBackend[Backend]
      .build
}
