// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.observationtree

import scala.collection.immutable.SortedSet
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
import explore.model.Constants
import explore.model.Focused
import explore.model.Focused._
import explore.model.ObsSummary
import explore.optics.GetAdjust
import explore.optics._
import explore.undo.KIListMod
import explore.undo.Undoer
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.model.Target
import monocle.Getter
import monocle.function.Field1.first
import mouse.boolean._
import react.beautifuldnd._
import react.common._
import react.common.implicits._
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.Button.ButtonProps
import react.semanticui.elements.icon.Icon
import react.semanticui.elements.segment.Segment
import react.semanticui.sizes._

import TargetObsQueries._

final case class TargetObsList(
  targetsWithObs:    View[TargetsWithObs],
  focused:           View[Option[Focused]],
  expandedTargetIds: View[SortedSet[Target.Id]]
) extends ReactProps[TargetObsList](TargetObsList.component)

object TargetObsList {
  type Props = TargetObsList

  val obsListMod    =
    new KIListMod[IO, ObsIdNameTarget, Observation.Id](ObsIdNameTarget.id)
  val targetListMod = new KIListMod[IO, TargetIdName, Target.Id](TargetIdName.id)

  class Backend($ : BackendScope[Props, Unit]) {

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
      targetsWithObs:        View[TargetsWithObs],
      obsId:                 Observation.Id,
      obsWithIndexGetAdjust: GetAdjust[ObsList, obsListMod.ElemWithIndex]
    ): Option[TargetIdName] => IO[Unit] =
      targetOpt => {
        val obsTargetAdjuster = obsWithIndexGetAdjust
          .composeOptionLens(first) // Focus on Observation within ElemWithIndex
          .composeOptionLens(ObsIdNameTarget.target)

        val observationsView = targetsWithObs
          .zoom(TargetsWithObs.obs)

        val oldTarget = obsTargetAdjuster.get(observationsView.get)

        // 1) Update internal model
        observationsView.mod(obsTargetAdjuster.set(targetOpt)) >>
          // 2) Send mutation
          oldTarget
            .flatMap(oldTarget =>
              targetOpt.map(newTarget => moveObs(obsId, oldTarget.id, newTarget.id))
            )
            .orEmpty
      }

    protected def onDragEnd(
      setter:            Undoer.Setter[IO, TargetsWithObs],
      expandedTargetIds: View[SortedSet[Target.Id]]
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
                  setTargetForObsWithId(props.targetsWithObs, obsId, obsWithId)
                )

            (expandedTargetIds.mod(_ + newTargetId) >> set(target.some)).runAsyncCB
          }).getOrEmpty
        }

    private def setTargetWithIndex(
      targetsWithObs:        View[TargetsWithObs],
      focused:               View[Option[Focused]],
      targetId:              Target.Id,
      targetWithIndexSetter: Adjuster[TargetList, targetListMod.ElemWithIndex],
      nextToFoucs:           Option[TargetIdName]
    ): targetListMod.ElemWithIndex => IO[Unit] =
      targetWithIndex =>
        // 1) Update internal model
        targetsWithObs
          .zoom(TargetsWithObs.targets)
          .mod(targetWithIndexSetter.set(targetWithIndex)) >>
          // 2) Send mutation & adjust focus
          targetWithIndex.fold(
            focused.set(nextToFoucs.map(f => Focused.FocusedTarget(f.id))) *> removeTarget(targetId)
          ) { case (target, _) =>
            insertTarget(target) *> focused.set(FocusedTarget(targetId).some)
          }

    private def targetMod(
      setter:         Undoer.Setter[IO, TargetsWithObs],
      targetsWithObs: View[TargetsWithObs],
      focused:        View[Option[Focused]],
      targetId:       Target.Id,
      focusOnDelete:  Option[TargetIdName]
    ): targetListMod.Operation => IO[Unit] = {
      val targetWithId: GetAdjust[TargetList, targetListMod.ElemWithIndex] =
        targetListMod.withKey(targetId)

      setter
        .mod[targetListMod.ElemWithIndex](
          targetsWithObs.get,
          TargetsWithObs.targets
            .composeGetter(targetWithId.getter)
            .get,
          setTargetWithIndex(targetsWithObs,
                             focused,
                             targetId,
                             targetWithId.adjuster,
                             focusOnDelete
          )
        )
    }

    protected def newTarget(setter: Undoer.Setter[IO, TargetsWithObs]): Callback =
      $.props >>= { props =>
        // Temporary measure until we have id pools.
        val newTarget =
          TargetIdName(Target.Id(PosLong.unsafeFrom(Random.nextInt().abs.toLong + 1)),
                       Constants.UnnamedTarget
          )

        val upsert =
          targetListMod
            .upsert(newTarget, props.targetsWithObs.get.targets.length)

        targetMod(setter, props.targetsWithObs, props.focused, newTarget.id, none)(
          upsert
        ).runAsyncCB
      }

    protected def deleteTarget(
      targetId:      Target.Id,
      setter:        Undoer.Setter[IO, TargetsWithObs],
      focusOnDelete: Option[TargetIdName]
    ): Callback =
      $.props.flatMap { props =>
        targetMod(setter, props.targetsWithObs, props.focused, targetId, focusOnDelete)(
          targetListMod.delete
        ).runAsyncCB
      }

    def toggleExpanded(
      targetId:          Target.Id,
      expandedTargetIds: View[SortedSet[Target.Id]]
    ): Callback =
      expandedTargetIds.mod { expanded =>
        expanded
          .exists(_ === targetId)
          .fold(expanded - targetId, expanded + targetId)
      }.runAsyncCB

    // Adapted from https://github.com/atlassian/react-beautiful-dnd/issues/374#issuecomment-569817782
    def getObsStyle(style:       TagMod, snapshot: Draggable.StateSnapshot): TagMod =
      if (!snapshot.isDragging)
        TagMod.empty
      else if (!snapshot.isDropAnimating)
        style
      else
        TagMod(style, ^.transitionDuration := "0.001s")

    def getListStyle(isDragging: Boolean): TagMod =
      ExploreStyles.DraggingOver.when(isDragging)

    def render(props: Props): VdomElement = {
      val obsByTarget = props.targetsWithObs.get.obs.toList.groupBy(_.target.id)

      val targets    = props.targetsWithObs.get.targets.toList
      val targetIds  = targets.map(_.id)
      val targetIdxs = targets.zipWithIndex

      React.Fragment(
        UndoRegion[TargetsWithObs] { undoCtx =>
          DragDropContext(onDragEnd = onDragEnd(undoCtx.setter, props.expandedTargetIds))(
            <.div(ExploreStyles.ObsTreeWrapper)(
              <.div(ExploreStyles.TreeToolbar)(
                <.div(
                  Button(size = Mini, compact = true, onClick = newTarget(undoCtx.setter))(
                    Icons.New.size(Small).fitted(true)
                  )
                ),
                UndoButtons(props.targetsWithObs.get, undoCtx, size = Mini)
              ),
              <.div(ExploreStyles.ObsTree)(
                <.div(ExploreStyles.ObsScrollTree)(
                  targets
                    .toTagMod { target =>
                      val targetId      = target.id
                      val currIdx       = targetIds.indexOf(targetId)
                      val nextToSelect  = targetIdxs.find(_._2 === currIdx + 1).map(_._1)
                      val prevToSelect  = targetIdxs.find(_._2 === currIdx - 1).map(_._1)
                      val focusOnDelete = nextToSelect.orElse(prevToSelect)

                      val targetObs = obsByTarget.get(targetId).orEmpty
                      val obsCount  = targetObs.length

                      val opIcon =
                        targetObs.nonEmpty.fold(
                          Icon(
                            "chevron " + props.expandedTargetIds.get
                              .exists(_ === targetId)
                              .fold("down", "right")
                          )(^.cursor.pointer,
                            ^.onClick ==> { e: ReactEvent =>
                              e.stopPropagationCB >>
                                toggleExpanded(targetId, props.expandedTargetIds)
                                  .asEventDefault(e)
                                  .void
                            }
                          ),
                          Icons.ChevronRight
                        )

                      val memberObsSelected = props.focused.get
                        .exists(f => targetObs.map(obs => FocusedObs(obs.id)).exists(f === _))
                      Droppable(target.id.toString) { case (provided, snapshot) =>
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
                                  .exists(_ === FocusedTarget(target.id))
                              )(ExploreStyles.SelectedObsTreeGroup)
                              .getOrElse(ExploreStyles.UnselectedObsTreeGroup)
                          )(
                            ^.cursor.pointer,
                            ^.onClick --> props.focused.set(FocusedTarget(targetId).some).runAsyncCB
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
                                clazz = ExploreStyles.DeleteButton |+| ExploreStyles.JustifyRight,
                                onClickE = (e: ReactMouseEvent, _: ButtonProps) =>
                                  e.stopPropagationCB *>
                                    deleteTarget(targetId, undoCtx.setter, focusOnDelete)
                              )(
                                Icons.Delete
                                  .size(Small)
                                  .fitted(true)
                                  .clazz(ExploreStyles.TrashIcon)
                              ),
                              <.span(ExploreStyles.ObsCount, s"$obsCount Obs")
                            ),
                            TagMod.when(props.expandedTargetIds.get.contains(targetId))(
                              targetObs.zipWithIndex.toTagMod { case (obs, idx) =>
                                <.div(ExploreStyles.ObsTreeItem)(
                                  Draggable(obs.id.toString, idx) { case (provided, snapshot, _) =>
                                    <.div(
                                      provided.innerRef,
                                      provided.draggableProps,
                                      getObsStyle(provided.draggableStyle, snapshot),
                                      ^.onClick ==> { e: ReactEvent =>
                                        e.stopPropagationCB >>
                                          props.focused
                                            .set(FocusedObs(obs.id).some)
                                            .runAsyncCB
                                      }
                                    )(
                                      <.span(provided.dragHandleProps)(
                                        ObsBadge(
                                          ObsSummary(obs.id, obs.target.name.value),
                                          ObsBadge.Layout.ConfAndConstraints,
                                          selected =
                                            props.focused.get.exists(_ === FocusedObs(obs.id))
                                        )
                                      )
                                    )
                                  }
                                )
                              }
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
    }
  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .renderBackend[Backend]
      .componentDidMount { $ =>
        // Remove targets from expanded set which are no longer present.
        ($.props.expandedTargetIds.get -- $.props.targetsWithObs.get.targets.toList
          .map(_.id)).toNes
          .map(removedTargetIds =>
            $.props.expandedTargetIds.mod(_ -- removedTargetIds.toSortedSet).runAsyncCB
          )
          .getOrEmpty
      }
      .build
}
