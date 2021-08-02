package explore.observationtree

import cats.effect.IO
import cats.effect.SyncIO
import cats.Order._
import cats.syntax.all._
import clue.TransactionalClient
import crystal.ViewF
import crystal.react.implicits._
import explore.Icons
import explore.common.ConstraintGroupQueries._
import explore.components.ui.ExploreStyles
import explore.components.undo.UndoButtons
import explore.implicits._
import explore.model.Focused
import explore.model.Focused._
import explore.model.reusability._
import explore.schemas.ObservationDB
import explore.undo._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.ui.reusability._
import monocle.Focus
import monocle.Lens
import mouse.boolean._
import react.beautifuldnd._
import react.common._
import react.common.implicits._
import react.fa.FontAwesomeIcon
import react.semanticui.elements.button.Button
import react.semanticui.elements.segment.Segment
import react.semanticui.sizes._
import scala.collection.immutable.SortedSet

final case class ConstraintGroupObsList(
  constraintsWithObs: View[ConstraintSummaryWithObervations],
  focused:            View[Option[Focused]],
  expandedIds:        View[SortedSet[SortedSet[Observation.Id]]],
  undoStacks:         View[UndoStacks[IO, ConstraintGroupList]]
)(implicit val ctx:   AppContextIO)
    extends ReactProps[ConstraintGroupObsList](ConstraintGroupObsList.component)
    with ViewCommon

object ConstraintGroupObsList {
  type Props = ConstraintGroupObsList

  case class State(dragging: Boolean = false)
  object State {
    val dragging: Lens[State, Boolean] = Focus[State](_.dragging)
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive
  implicit val stateReuse: Reusability[State] = Reusability.derive

  class Backend($ : BackendScope[Props, State]) {
    def obsIdsToString(obsIds: SortedSet[Observation.Id]): String = obsIds.mkString(",")

    def obsIdStringToIds(obsIdStr: String): Option[SortedSet[Observation.Id]] =
      obsIdStr.split(",").toList.map(Observation.Id.parse(_)).sequence.map(SortedSet.from(_))

    def toggleExpanded(
      obsIds:      SortedSet[Observation.Id],
      expandedIds: View[SortedSet[SortedSet[Observation.Id]]]
    ): SyncIO[Unit] =
      expandedIds.mod { expanded =>
        expanded.exists(_ === obsIds).fold(expanded - obsIds, expanded + obsIds)
      }

    def onDragEnd(
      undoCtx:     UndoCtx[ConstraintGroupList],
      expandedIds: View[SortedSet[SortedSet[Observation.Id]]]
    )(implicit
      c:           TransactionalClient[IO, ObservationDB]
    ): (DropResult, ResponderProvided) => SyncIO[Unit] = (result, _) =>
      $.propsIn[SyncIO].flatMap { props =>
        val oData = for {
          destination <- result.destination.toOption
          destIds     <- obsIdStringToIds(destination.droppableId)
          obsId       <- Observation.Id.parse(result.draggableId)
          if !destIds.contains(obsId)
          destCg      <- props.constraintsWithObs.get.constraintGroups.get(destIds)
        } yield (destCg, obsId)

        oData.fold(SyncIO.unit) { case (destCg, obsId) =>
          ConstraintGroupObsListActions
            .obsConstraintGroup[IO](obsId, expandedIds)
            .set(undoCtx)(destCg.constraintSet.some)
        }
      }

    def render(props: Props) = {
      implicit val ctx = props.ctx

      val observations = props.constraintsWithObs.get.observations

      val constraintGroups = props.constraintsWithObs.get.constraintGroups

      val state   = ViewF.fromStateSyncIO($)
      val undoCtx = UndoContext(
        props.undoStacks,
        props.constraintsWithObs.zoom(ConstraintSummaryWithObervations.constraintGroups)
      )

      val renderClone: Draggable.Render = (provided, snapshot, rubric) => {
        <.div(provided.innerRef,
              provided.draggableProps,
              provided.dragHandleProps,
              props.getDraggedStyle(provided.draggableStyle, snapshot)
        )(
          Observation.Id
            .parse(rubric.draggableId)
            .flatMap(obsId => observations.get(obsId))
            .map(obs => props.renderObsBadge(obs))
            .getOrElse(<.span("ERROR"))
        )
      }

      val handleDragEnd = onDragEnd(undoCtx, props.expandedIds)

      DragDropContext(
        onDragStart =
          (_: DragStart, _: ResponderProvided) => state.zoom(State.dragging).set(true).toCB,
        onDragEnd = (result, provided) =>
          state.zoom(State.dragging).set(false) >> handleDragEnd(result, provided)
      )(
        <.div(ExploreStyles.ObsTreeWrapper)(
          <.div(ExploreStyles.TreeToolbar)(UndoButtons(undoCtx, size = Mini)),
          <.div(
            Button(onClick = props.focused.set(none), clazz = ExploreStyles.ButtonSummary)(
              Icons.ListIcon.clazz(ExploreStyles.PaddedRightIcon),
              "Constraints Summary"
            )
          ),
          <.div(ExploreStyles.ObsTree)(
            <.div(ExploreStyles.ObsScrollTree)(
              constraintGroups.toTagMod { case (_, constraintGroup) =>
                val obsIds      = constraintGroup.obsIds
                val cgObs       = obsIds.toList.map(id => observations.get(id)).flatten
                val obsSelected =
                  props.focused.get.exists(f =>
                    obsIds.unsorted.map(id => FocusedObs(id)).exists(f === _)
                  )
                val cgSelected  = props.focused.get.exists(_ === FocusedConstraintGroup(obsIds))

                val icon: FontAwesomeIcon = props.expandedIds.get
                  .exists((ids: SortedSet[Observation.Id]) => ids === obsIds)
                  .fold(Icons.ChevronDown, Icons.ChevronRight)
                  .addModifiers(
                    Seq(^.cursor.pointer,
                        ^.onClick ==> { e: ReactEvent =>
                          e.stopPropagationCB >> toggleExpanded(obsIds, props.expandedIds).toCB
                            .asEventDefault(e)
                            .void
                        }
                    )
                  )
                  .fixedWidth()

                Droppable(obsIdsToString(obsIds), renderClone = renderClone) {
                  case (provided, snapshot) =>
                    val csHeader = <.span(ExploreStyles.ObsTreeGroupHeader)(
                      <.span(ExploreStyles.ObsGroupTitle)(
                        icon,
                        constraintGroup.constraintSet.name.value
                      ),
                      <.span(ExploreStyles.ObsCount, s"${obsIds.size} Obs")
                    )

                    <.div(
                      provided.innerRef,
                      provided.droppableProps,
                      props.getListStyle(
                        snapshot.draggingOverWith.exists(id => Observation.Id.parse(id).isDefined)
                      )
                    )(
                      Segment(
                        vertical = true,
                        clazz = ExploreStyles.ObsTreeGroup |+| Option
                          .when(obsSelected || cgSelected)(ExploreStyles.SelectedObsTreeGroup)
                          .orElse(
                            Option.when(!state.get.dragging)(ExploreStyles.UnselectedObsTreeGroup)
                          )
                          .orEmpty
                      )(^.cursor.pointer,
                        ^.onClick --> props.focused.set(FocusedConstraintGroup(obsIds).some)
                      )(
                        csHeader,
                        TagMod.when(props.expandedIds.get.contains(obsIds))(
                          cgObs.zipWithIndex.toTagMod { case (obs, idx) =>
                            props.renderObsBadgeItem(selectable = false)(obs, idx)
                          }
                        )
                      )
                    )
                }
              }
            )
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
      val constraintsWithObs = $.props.constraintsWithObs.get
      val constraintGroups   = constraintsWithObs.constraintGroups
      val observations       = constraintsWithObs.observations
      val expandedIds        = $.props.expandedIds

      // Unfocus if focused element is not there
      val unfocus = $.props.focused.mod(_.flatMap {
        case FocusedObs(obsId) if !observations.contains(obsId)                   => none
        case FocusedConstraintGroup(obsIds) if !constraintGroups.contains(obsIds) => none
        case other                                                                => other.some
      })

      // Expand constraint group with focused observation
      val expandForObservation = $.props.focused.get
        .collect { case FocusedObs(obsId) =>
          constraintGroups.find(_._1.contains(obsId)).map { case (ids, _) =>
            expandedIds.mod(_ + ids)
          }
        }
        .flatten
        .orEmpty

      val cleanupExpandedIds =
        expandedIds.mod(_.filter(ids => constraintGroups.contains(ids)))

      unfocus >> expandForObservation >> cleanupExpandedIds
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
