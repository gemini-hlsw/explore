// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import java.util.UUID

import scala.collection.immutable.HashSet

import cats.effect.IO
import cats.implicits._
import crystal.react.implicits._
import explore.Icons
import explore.components.ObsBadge
import explore.components.ui.GPPStyles
import explore.components.undo.UndoButtons
import explore.components.undo.UndoRegion
import explore.implicits._
import explore.model.ExploreObservation
import explore.model.Focused
import explore.model.Focused.FocusedObs
import explore.model.Focused.FocusedTarget
import explore.model.SiderealTarget
import explore.undo.ListMod
import explore.undo.Undoer
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.effects.CallbackToEffects._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.Getter
import monocle.Lens
import monocle.Setter
import monocle.function.Field1.first
import monocle.macros.Lenses
import monocle.std.option.some
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
  case class State(collapsedTargetIds: Set[SiderealTarget.Id] = HashSet.empty)

  val obsListMod    = new ListMod[IO, ExploreObservation, ExploreObservation.Id](ExploreObservation.id)
  val targetListMod = new ListMod[IO, SiderealTarget, SiderealTarget.Id](SiderealTarget.id)

  implicit class LensOptionOps[S, A](val lens: Lens[S, Option[A]]) extends AnyVal {
    def composeOptionLens[B](other: Lens[A, B]): Lens[S, Option[B]] =
      Lens(
        lens.composePrism(some).composeLens(other).getOption
      )(
        _.fold(lens.set(none))(b => lens.composePrism(some).composeLens(other).set(b))
      )
  }

  implicit class GetterOptionOps[S, A](val getter: Getter[S, Option[A]]) extends AnyVal {
    def composeOptionLens[B](other: Lens[A, B]): Getter[S, Option[B]] =
      Getter(
        getter.composePrism(some).composeLens(other).headOption
      )
  }

  implicit class SetterOptionOps[S, A](val setter: Setter[S, Option[A]]) extends AnyVal {
    def composeOptionLens[B](other: Lens[A, B]): Setter[S, Option[B]] =
      Setter { modOptB: (Option[B] => Option[B]) =>
        setter.modify { optA =>
          optA.flatMap[A] { a =>
            modOptB(other.get(a).some).map(b => other.set(b)(a))
          }
        }
      }
  }

  class Backend($ : BackendScope[Props, State]) {

    protected def onDragEnd(
      setter: Undoer.Setter[IO, TargetsWithObs]
    ): (DropResult, ResponderProvided) => Callback =
      (result, _) =>
        $.props >>= { props =>
          (for {
            newTargetIdStr <- result.destination.toOption.map(_.droppableId)
            newTargetId     = UUID.fromString(newTargetIdStr)
            target         <- props.targetsWithObs.get.targets.find(_.id === newTargetId)
          } yield {
            val obsId = UUID.fromString(result.draggableId)

            val getSetWithId =
              obsListMod
                .withId(obsId)

            val set =
              setter
                .set[Option[SiderealTarget]](
                  props.targetsWithObs.get,
                  TargetsWithObs.obs
                    .composeGetter(
                      getSetWithId.getter
                        .composeOptionLens(first)
                        .composeOptionLens(ExploreObservation.target)
                    )
                    .get,
                  { value: Option[SiderealTarget] =>
                    props.targetsWithObs
                      .zoom(TargetsWithObs.obs)
                      .mod( // 1) Update internal model
                        getSetWithId.setter
                          .composeOptionLens(first)
                          .composeOptionLens(ExploreObservation.target)
                          .set(value)
                      ) >>
                      // 2) Send mutation
                      mutateObs(obsId, ObsMutation.Fields(target_id = value.map(_.id)))
                  }
                ) _

            set(target.some).runInCB
          }).getOrEmpty
        }

    protected def targetMod(
      setter:         Undoer.Setter[IO, TargetsWithObs],
      targetsWithObs: View[TargetsWithObs],
      focused:        View[Option[Focused]],
      targetId:       SiderealTarget.Id
    ): targetListMod.Operation => IO[Unit] = {
      val getSetWithId = targetListMod.withId(targetId)

      setter
        .mod[targetListMod.ElemWithIndex](
          targetsWithObs.get,
          TargetsWithObs.targets
            .composeGetter(getSetWithId.getter)
            .get,
          { value: targetListMod.ElemWithIndex =>
            targetsWithObs
              .zoom(TargetsWithObs.targets)
              .mod(getSetWithId.setter.set(value)) >>
              // 2) Send mutation
              value.fold(focused.set(none) >> removeTarget(targetId)) {
                case (target, _) =>
                  insertTarget(target) >> focused.set(FocusedTarget(targetId).some)
              }
          }
        )
    }

    protected def newTarget(setter: Undoer.Setter[IO, TargetsWithObs]): Callback =
      $.props >>= { props =>
        SiderealTarget.createNew[CallbackTo] >>= { newTarget =>
          val upsert =
            targetListMod
              .upsert(newTarget, props.targetsWithObs.get.targets.length)

          targetMod(setter, props.targetsWithObs, props.focused, newTarget.id)(upsert).runInCB
        }
      }

    protected def deleteTargetEnabled(
      focused:        Option[Focused],
      targetsWithObs: TargetsWithObs
    ): Option[SiderealTarget.Id] =
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

    def toggleCollapsed(targetId: SiderealTarget.Id): Callback =
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
      GPPStyles.DraggingOver.when(isDragging)

    def render(props: Props, state: State): VdomElement = {
      val obsByTarget = props.targetsWithObs.get.obs.groupBy(_.target)

      <.div(GPPStyles.ObsTree)(
        UndoRegion[TargetsWithObs] { undoCtx =>
          DragDropContext(onDragEnd = onDragEnd(undoCtx.setter))(
            <.div(
              props.targetsWithObs.get.targets.toTagMod {
                target =>
                  val targetId = target.id

                  val targetObs = obsByTarget.getOrElse(target, List.empty)
                  val obsCount  = targetObs.length

                  val opIcon =
                    targetObs.nonEmpty.fold(
                      Icon(
                        "chevron " + state.collapsedTargetIds
                          .exists(_ === targetId)
                          .fold("right", "down")
                      )(^.cursor.pointer,
                        ^.onClick ==> { e: ReactEvent =>
                          e.stopPropagationCB >> toggleCollapsed(targetId)
                        }
                      ),
                      Icon("chevron right")
                    )

                  Droppable(target.id.toString) {
                    case (provided, snapshot) =>
                      <.div(
                        provided.innerRef,
                        provided.droppableProps,
                        getListStyle(snapshot.isDraggingOver)
                      )(
                        Segment(vertical = true,
                                raised = props.focused.get
                                  .exists(_ === FocusedTarget(target.id)),
                                clazz = GPPStyles.ObsTreeGroup
                        )(
                          ^.cursor.pointer,
                          ^.onClick --> props.focused.set(FocusedTarget(targetId).some).runInCB
                        )(
                          <.span(GPPStyles.ObsTreeGroupHeader)(
                            <.span(
                              opIcon,
                              target.name
                            ),
                            <.span(^.float.right, s"$obsCount Obs")
                          ),
                          TagMod.when(!state.collapsedTargetIds.contains(targetId))(
                            targetObs.zipWithIndex.toTagMod {
                              case (obs, idx) =>
                                <.div(GPPStyles.ObsTreeItem)(
                                  Draggable(obs.id.toString, idx) {
                                    case (provided, snapshot, _) =>
                                      def dragIcon =
                                        <.span(
                                          provided.dragHandleProps,
                                          Icon("sort")
                                        )

                                      <.div(
                                        provided.innerRef,
                                        provided.draggableProps,
                                        getObsStyle(provided.draggableStyle, snapshot),
                                        ^.cursor.pointer,
                                        ^.onClick ==> { e: ReactEvent =>
                                          e.stopPropagationCB >>
                                            props.focused
                                              .set(FocusedObs(obs.id).some)
                                              .runInCB
                                        }
                                      )(
                                        decorateTopRight(
                                          ObsBadge(obs,
                                                   ObsBadge.Layout.ConfAndConstraints,
                                                   selected = props.focused.get
                                                     .exists(_ === FocusedObs(obs.id))
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
            <.div(GPPStyles.ObsTreeButtons)(
              <.div(
                Button(size = Small, onClick = newTarget(undoCtx.setter))(Icons.New),
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
      .backend(new Backend(_))
      .renderBackend
      .build
}
