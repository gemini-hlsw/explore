// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

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
import explore.model.SelectedPanel
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
import explore.model.SelectedPanel.Uninitialized
import explore.model.SelectedPanel.Editor

final case class ConstraintGroupObsList(
  constraintsWithObs: View[ConstraintSummaryWithObervations],
  focused:            View[Option[Focused]],
  selected:           View[SelectedPanel[ConstraintGroup]],
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
            Button(onClick = props.selected.set(SelectedPanel.summary),
                   clazz = ExploreStyles.ButtonSummary
            )(
              Icons.ListIcon.clazz(ExploreStyles.PaddedRightIcon),
              "Constraints Summary"
            )
          ),
          <.div(ExploreStyles.ObsTree)(
            <.div(ExploreStyles.ObsScrollTree)(
              constraintGroups.toTagMod {
                case (_, constraintGroup) =>
                  val obsIds        = constraintGroup.obsIds
                  val cgObs         = obsIds.toList.map(id => observations.get(id)).flatten
                  val groupSelected = props.selected.get.optValue.exists(_.obsIds === obsIds)

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
                        icon,
                        <.span(ExploreStyles.ObsGroupTitleWithWrap)(
                          constraintGroup.constraintSet.displayName
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
                            .when(groupSelected)(ExploreStyles.SelectedObsTreeGroup)
                            .orElse(
                              Option.when(!state.get.dragging)(ExploreStyles.UnselectedObsTreeGroup)
                            )
                            .orEmpty
                        )(^.cursor.pointer,
                          ^.onClick --> props.selected.set(SelectedPanel.editor(constraintGroup))
                        )(
                          csHeader,
                          TagMod.when(props.expandedIds.get.contains(obsIds))(
                            cgObs.zipWithIndex.toTagMod { case (obs, idx) =>
                              props.renderObsBadgeItem(selectable = false,
                                                       highlightSelected = false
                              )(obs, idx)
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
      val selected           = $.props.selected

      // Unfocus if focused element is not there
      val unfocus = $.props.focused.mod(_.flatMap {
        case FocusedObs(obsId) if !observations.contains(obsId) => none
        case other                                              => other.some
      })

      val setAndGetSelected = selected.get match {
        case Uninitialized =>
          val constraintGroupFromFocused = $.props.focused.get.collect { case FocusedObs(obsId) =>
            constraintGroups.find(_._1.contains(obsId)).map(_._2)
          }.flatten

          selected
            .set(
              constraintGroupFromFocused.fold(SelectedPanel.tree[ConstraintGroup])(cg =>
                SelectedPanel.editor(cg)
              )
            )
            .as(constraintGroupFromFocused)
        case Editor(cg)    => SyncIO.delay(cg.some)
        case _             => SyncIO.delay(none)
      }

      def expandSelected(cgOpt: Option[ConstraintGroup]) =
        cgOpt
          .map(cg => expandedIds.mod(_ + cg.obsIds))
          .orEmpty

      val cleanupExpandedIds =
        expandedIds.mod(_.filter(ids => constraintGroups.contains(ids)))
      for {
        _     <- unfocus
        cgOpt <- setAndGetSelected
        _     <- expandSelected(cgOpt)
        _     <- cleanupExpandedIds
      } yield ()
    }
    .configure(Reusability.shouldComponentUpdate)
    .build
}
