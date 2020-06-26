package explore.observationtree

import explore.implicits._
import cats.implicits._
import explore.model.SiderealTarget
import explore.model.ExploreObservation
import react.common.ReactProps
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.MonocleReact._
import scalajs.js.|
import explore.components.ObsBadge
import scala.collection.immutable.HashSet
import monocle.macros.Lenses
import mouse.boolean._
import react.semanticui.elements.icon.Icon
import react.beautifuldnd._
import crystal.react.implicits._
import explore.components.undo.UndoRegion
import react.semanticui.elements.button.Button
import explore.undo.ListMod
import cats.effect.IO
import java.util.UUID
import explore.undo.Undoer
import monocle.function.Field1.first
import monocle.function.Possible.possible
import monocle.std.option.some
import monocle.Lens
import monocle.Getter
import monocle.Setter
import explore.model.Focused
import gem.Observation
import explore.components.ui.GPPStyles
import explore.components.undo.UndoButtons
import TargetObsQueries._

final case class TargetObsList(
  targets:      List[SiderealTarget],
  observations: View[List[ExploreObservation]],
  focused:      ViewOpt[Either[SiderealTarget.Id, ExploreObservation.Id]]
) extends ReactProps[TargetObsList](TargetObsList.component)

object TargetObsList {
  type Props = TargetObsList

  @Lenses
  case class State(collapsedTargetIds: Set[SiderealTarget.Id] = HashSet.empty)

  val obsListMod = new ListMod[IO, ExploreObservation, ExploreObservation.Id](ExploreObservation.id)

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
    def onDragEnd(
      setter: Undoer.Setter[IO, List[ExploreObservation]]
    ): (DropResult, ResponderProvided) => Callback =
      (result, _) =>
        $.props >>= { props =>
          (for {
            newTargetId <- result.destination.toOption.map(_.droppableId)
            target      <- props.targets.find(_.name === newTargetId)
          } yield {
            val obsId = UUID.fromString(result.draggableId)

            val getSetWithId =
              obsListMod
                .withId(obsId)

            val set =
              setter
                .set[Option[SiderealTarget]](
                  props.observations.get,
                  getSetWithId.getter
                    .composeOptionLens(first)
                    .composeOptionLens(ExploreObservation.target)
                    .get,
                  { value: Option[SiderealTarget] =>
                    props.observations.mod( // 1) Update internal model
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

    def render(props: Props, state: State): VdomElement = {
      val observations = props.observations.get
      val obsByTarget  = observations.groupBy(_.target)

      <.div(GPPStyles.ObsTree)(
        UndoRegion[List[ExploreObservation]] { undoCtx =>
          DragDropContext(onDragEnd = onDragEnd(undoCtx.setter))(
            <.div(
              UndoButtons(observations, undoCtx),
              props.targets.toTagMod {
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
                      )(^.cursor.pointer, ^.onClick --> toggleCollapsed(targetId)),
                      Icon("chevron right")
                    )

                  Droppable(target.name) {
                    case (provided, _ /*snapshot*/ ) =>
                      <.div(
                        provided.innerRef,
                        provided.droppableProps
                        // getListStyle(snapshot.isDraggingOver)
                      )(
                        <.span(
                          opIcon,
                          <.span(
                            target.name,
                            <.span(^.float.right, s"$obsCount Obs"),
                            ^.cursor.pointer,
                            ^.onClick --> props.focused.set(targetId.asLeft).runInCB
                          )
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
                                      ^.onClick --> props.focused.set(obs.id.asRight).runInCB
                                    )(
                                      decorateTopRight(
                                        ObsBadge(obs,
                                                 ObsBadge.Layout.ConfAndConstraints,
                                                 selected = props.focused.get
                                                   .flatMap(_.toOption)
                                                   .exists(_ === obs.id)
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
                  }
              }
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
