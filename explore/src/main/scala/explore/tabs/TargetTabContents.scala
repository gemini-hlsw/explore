// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.data.NonEmptySet
import cats.effect.IO
import cats.syntax.all._
import crystal.ViewF
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.common.TargetListGroupQueries._
import explore.common.UserPreferencesQueries._
import explore.common.UserPreferencesQueriesGQL._
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model._
import explore.model.reusability._
import explore.observationtree.TargetListGroupObsList
import explore.undo._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.builder.Lifecycle.ComponentDidMount
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Target
import lucuma.core.model.TargetEnvironment
import lucuma.core.model.User
import lucuma.ui.reusability._
import org.scalajs.dom.window
import react.common._
import react.common.implicits._
import react.draggable.Axis
import react.resizable._
import react.resizeDetector.ResizeDetector

import scala.collection.immutable.SortedSet
import scala.concurrent.duration._

final case class TargetTabContents(
  userId:           Option[User.Id],
  focused:          View[Option[Focused]],
  listUndoStacks:   View[UndoStacks[IO, TargetListGroupList]],
  // targetsUndoStacks: View[Map[Target.Id, UndoStacks[IO, TargetResult]]],
  // searching:         View[Set[Target.Id]],
  expandedIds:      View[SortedSet[NonEmptySet[TargetEnvironment.Id]]],
  hiddenColumns:    View[Set[String]],
  size:             ResizeDetector.Dimensions
)(implicit val ctx: AppContextIO)
    extends ReactProps[TargetTabContents](TargetTabContents.component) {
  def selectedPanel: SelectedPanel[Target.Id] = focused.get
    .collect { case Focused.FocusedTarget(id) =>
      id
    }
    .fold(SelectedPanel.tree[Target.Id])(SelectedPanel.editor)
}

object TargetTabContents {
  type Props = TargetTabContents
  type State = TwoPanelState[NonEmptySet[TargetEnvironment.Id]]

  implicit val propsReuse: Reusability[Props] = Reusability.derive

  val treeWidthLens = TwoPanelState.treeWidth[NonEmptySet[TargetEnvironment.Id]]
  val selectedLens  = TwoPanelState.selected[NonEmptySet[TargetEnvironment.Id]]

  def readWidthPreference($ : ComponentDidMount[Props, State, _]): Callback = {
    implicit val ctx = $.props.ctx
    (UserAreaWidths.queryWithDefault[IO]($.props.userId,
                                         ResizableSection.TargetsTree,
                                         Constants.InitialTreeWidth.toInt
    ) >>= $.setStateLIn[IO](TwoPanelState.treeWidth)).runAsyncCB
  }

  protected def renderFn(
    props:                  Props,
    state:                  View[State],
    targetListGroupWithObs: View[TargetListGroupWithObs]
  )(implicit ctx:           AppContextIO): VdomNode = {
    val treeResize =
      (_: ReactEvent, d: ResizeCallbackData) =>
        (state.zoom(treeWidthLens).set(d.size.width).to[IO] *>
          UserWidthsCreation
            .storeWidthPreference[IO](props.userId,
                                      ResizableSection.TargetsTree,
                                      d.size.width
            )).runAsyncCB
          .debounce(1.second)

    val treeWidth = state.get.treeWidth.toInt

    // Tree area
    def tree(objectsWithObs: View[TargetListGroupWithObs]) =
      <.div(^.width := treeWidth.px, ExploreStyles.Tree |+| ExploreStyles.ResizableSinglePanel)(
        treeInner(objectsWithObs)
      )

    def treeInner(objectsWithObs: View[TargetListGroupWithObs]) =
      <.div(ExploreStyles.TreeBody)(
        TargetListGroupObsList(
          objectsWithObs,
          props.focused,
          state.zoom(selectedLens),
          props.expandedIds,
          // props.searching,
          props.listUndoStacks
        )
      )

    def findTargetListGroup(
      targetEnvIds: NonEmptySet[TargetEnvironment.Id],
      tlgl:         TargetListGroupList
    ): Option[TargetEnv] = tlgl.values.find(_.targetEnvIds.intersect(targetEnvIds).nonEmpty)

    // def onModTargetsWithObs(
    //   groupObsIds:  SortedSet[Observation.Id],
    //   editedObsIds: SortedSet[Observation.Id]
    // )(tlgwo:        TargetListGroupWithObs): SyncIO[Unit] = {
    //   val groupList = tlgwo.targetListGroups

    //   // If we're editing at the group level (even a group of 1) and it no longer exists
    //   // (probably due to a merger), just go to the summary.
    //   val updateSelection = props.focused.get match {
    //     case Some(Focused.FocusedObs(_)) => SyncIO.unit
    //     case _                           =>
    //       groupList
    //         .get(editedObsIds)
    //         .fold(state.zoom(selectedLens).set(SelectedPanel.summary))(_ => SyncIO.unit)
    //   }

    //   val updateExpanded = findTargetListGroup(editedObsIds, groupList).fold(SyncIO.unit) { cg =>
    //     // We should always find the constraint group.
    //     // If a group was edited while closed and it didn't create a merger, keep it closed,
    //     // otherwise expand all affected groups.
    //     props.expandedIds
    //       .mod { eids =>
    //         val withOld       =
    //           if (groupObsIds === editedObsIds) eids
    //           else eids + (groupObsIds -- editedObsIds)
    //         val withOldAndNew =
    //           if (editedObsIds === cg.obsIds && editedObsIds === groupObsIds) withOld
    //           else withOld + cg.obsIds

    //         withOldAndNew.filter(ids => groupList.contains(ids)) // clean up
    //       }
    //   }

    //   updateSelection >> updateExpanded
    // }

    // val backButton = Reuse.always[VdomNode](
    //   Button(
    //     as = <.a,
    //     size = Mini,
    //     compact = true,
    //     basic = true,
    //     clazz = ExploreStyles.TileBackButton |+| ExploreStyles.BlendedButton,
    //     onClickE = linkOverride[ButtonProps](props.focused.set(none))
    //   )(^.href := ctx.pageUrl(AppTab.Targets, none), Icons.ChevronLeft)
    // )

    val coreWidth  = props.size.width.getOrElse(0) - treeWidth
    val coreHeight = props.size.height.getOrElse(0)

    val rightSide = state.get.selected.optValue
      .flatMap(ids =>
        findTargetListGroup(ids, targetListGroupWithObs.get.targetListGroups).map(tlg => (ids, tlg))
      )
      .fold[VdomNode](<.div("Summary table")) { case (idsToEdit, targetListGroup) =>
        <.div(
          <.div(s"Editor for ids: $idsToEdit"),
          <.div(s"In group with these ids: ${targetListGroup.targetEnvIds}")
        )
      }
    // (props.userId, targetIdOpt).tupled match {
    //   case Some((uid, tid)) =>
    //     Tile("target", s"Target", backButton.some)(
    //       Reuse(renderContents _)(
    //         uid,
    //         tid,
    //         props.targetsUndoStacks.zoom(atMapWithDefault(tid, UndoStacks.empty)),
    //         props.searching
    //       )
    //     )
    //   case None             =>
    //     Tile("target", s"Targets Summary", backButton.some)(
    //       Reuse.by((pointingsWithObs, props.hiddenColumns))((renderInTitle: Tile.RenderInTitle) =>
    //         TargetSummaryTable(pointingsWithObs.get,
    //                            props.hiddenColumns,
    //                            props.focused,
    //                            props.expandedIds,
    //                            renderInTitle
    //         )
    //       )
    //     )
    // }

    // It would be nice to make a single component here but it gets hard when you
    // have the resizable element. Instead we have either two panels with a resizable
    // or only one panel at a time (Mobile)
    if (window.innerWidth <= Constants.TwoPanelCutoff) {
      <.div(
        ExploreStyles.TreeRGL,
        <.div(ExploreStyles.Tree, treeInner(targetListGroupWithObs))
          .when(state.get.selected.leftPanelVisible),
        <.div(^.key := "target-right-side", ExploreStyles.SinglePanelTile)(
          rightSide
        ).when(state.get.selected.rightPanelVisible)
      )
    } else {
      <.div(
        ExploreStyles.TreeRGL,
        Resizable(
          axis = Axis.X,
          width = treeWidth,
          height = coreHeight,
          minConstraints = (Constants.MinLeftPanelWidth.toInt, 0),
          maxConstraints = (props.size.width.getOrElse(0) / 2, 0),
          onResize = treeResize,
          resizeHandles = List(ResizeHandleAxis.East),
          content = tree(targetListGroupWithObs),
          clazz = ExploreStyles.ResizableSeparator
        ),
        <.div(^.key   := "target-right-side",
              ExploreStyles.SinglePanelTile,
              ^.width := coreWidth.px,
              ^.left  := treeWidth.px
        )(
          rightSide
        )
      )
    }
  }

  protected class Backend($ : BackendScope[Props, State]) {
    def render(props: Props) = {
      implicit val ctx = props.ctx
      TargetListGroupLiveQuery(Reuse(renderFn _)(props, ViewF.fromStateSyncIO($)))
    }
  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .initialState(
        TwoPanelState.initial[NonEmptySet[TargetEnvironment.Id]](SelectedPanel.Uninitialized)
      )
      .renderBackend[Backend]
      .componentDidMount(readWidthPreference)
      .configure(Reusability.shouldComponentUpdate)
      .build

}
