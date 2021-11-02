// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all._
import crystal.ViewF
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.Icons
import explore.common.TargetListGroupQueries._
import explore.common.UserPreferencesQueries._
import explore.common.UserPreferencesQueriesGQL._
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model._
import explore.model.enum.AppTab
import explore.model.reusability._
import explore.observationtree.TargetListGroupObsList
import explore.targeteditor.TargetEnvEditor
import explore.targeteditor.TargetSummaryTable
import explore.undo._
import japgolly.scalajs.react._
import japgolly.scalajs.react.callback.CallbackCats._
import japgolly.scalajs.react.component.builder.Lifecycle.ComponentDidMount
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.SiderealTarget
import lucuma.core.model.User
import lucuma.ui.reusability._
import lucuma.ui.utils._
import monocle.Focus
import org.scalajs.dom.window
import react.common._
import react.common.implicits._
import react.draggable.Axis
import react.resizable._
import react.resizeDetector.ResizeDetector
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.Button.ButtonProps
import react.semanticui.sizes._

import scala.collection.immutable.SortedSet
import scala.concurrent.duration._

final case class TargetTabContents(
  userId:            Option[User.Id],
  focusedObs:        View[Option[FocusedObs]],
  listUndoStacks:    View[UndoStacks[IO, TargetListGroupList]],
  targetsUndoStacks: View[Map[TargetIdSet, UndoStacks[IO, SiderealTarget]]],
  searching:         View[Set[TargetIdSet]],
  expandedIds:       View[SortedSet[TargetEnvGroupIdSet]],
  hiddenColumns:     View[Set[String]],
  size:              ResizeDetector.Dimensions
)(implicit val ctx:  AppContextIO)
    extends ReactProps[TargetTabContents](TargetTabContents.component)

object TargetTabContents {
  type Props = TargetTabContents

  final case class State(
    panels:  TwoPanelState[TargetEnvGroupIdSet],
    options: TargetVisualOptions // TODO: not setting fov from user preferences, yet
  )

  object State {
    val panels         = Focus[State](_.panels)
    val options        = Focus[State](_.options)
    val panelsWidth    = State.panels.andThen(TwoPanelState.treeWidth[TargetEnvGroupIdSet])
    val panelsSelected = State.panels.andThen(TwoPanelState.selected[TargetEnvGroupIdSet])
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive
  implicit val stateReuse: Reusability[State] = Reusability.derive

  val treeWidthLens = TwoPanelState.treeWidth[TargetEnvGroupIdSet]
  val selectedLens  = TwoPanelState.selected[TargetEnvGroupIdSet]

  def readWidthPreference($ : ComponentDidMount[Props, State, _]): Callback = {
    implicit val ctx = $.props.ctx
    UserAreaWidths
      .queryWithDefault[IO]($.props.userId,
                            ResizableSection.TargetsTree,
                            Constants.InitialTreeWidth.toInt
      )
      .runAsyncAndThen {
        case Right(w) => $.modState(State.panelsWidth.replace(w))
        case Left(_)  => Callback.empty
      }
  }

  protected def renderFn(
    props:                  Props,
    state:                  View[State],
    targetListGroupWithObs: View[TargetListGroupWithObs]
  )(implicit ctx:           AppContextIO): VdomNode = {
    val treeResize =
      (_: ReactEvent, d: ResizeCallbackData) =>
        (state.zoom(State.panelsWidth).set(d.size.width).to[IO] *>
          UserWidthsCreation
            .storeWidthPreference[IO](props.userId,
                                      ResizableSection.TargetsTree,
                                      d.size.width
            )).runAsync
          .debounce(1.second)

    val treeWidth = state.get.panels.treeWidth.toInt

    // Tree area
    def tree(objectsWithObs: View[TargetListGroupWithObs]) =
      <.div(^.width := treeWidth.px, ExploreStyles.Tree |+| ExploreStyles.ResizableSinglePanel)(
        treeInner(objectsWithObs)
      )

    def treeInner(objectsWithObs: View[TargetListGroupWithObs]) =
      <.div(ExploreStyles.TreeBody)(
        TargetListGroupObsList(
          objectsWithObs,
          props.focusedObs,
          state.zoom(State.panelsSelected),
          props.expandedIds,
          props.listUndoStacks
        )
      )

    def findTargetListGroup(
      targetEnvGroupIds: TargetEnvGroupIdSet,
      tlgl:              TargetListGroupList
    ): Option[TargetEnvGroup] = tlgl.values.find(_.id.intersect(targetEnvGroupIds).nonEmpty)

    def onModTargetsWithObs(
      groupIds:  TargetEnvGroupIdSet,
      editedIds: TargetEnvGroupIdSet
    )(tlgwo:     TargetListGroupWithObs): Callback = {
      val groupList = tlgwo.targetListGroups

      // If we're editing at the group level (even a group of 1) and it no longer exists
      // (probably due to a merger), just go to the summary.
      val updateSelection = props.focusedObs.get match {
        case Some(_) => Callback.empty
        case _       =>
          groupList
            .get(editedIds)
            .fold(state.zoom(State.panelsSelected).set(SelectedPanel.summary))(_ => Callback.empty)
      }

      val updateExpanded = findTargetListGroup(editedIds, groupList).fold(Callback.empty) { tlg =>
        // We should always find the group.
        // If a group was edited while closed and it didn't create a merger, keep it closed,
        // otherwise expand all affected groups.
        props.expandedIds
          .mod { eids =>
            val withOld       =
              if (groupIds === editedIds) eids
              else eids + groupIds.removeUnsafe(editedIds)
            val withOldAndNew =
              if (editedIds === tlg.id && editedIds === groupIds) withOld
              else withOld + tlg.id

            withOldAndNew.filter(ids => groupList.contains(ids)) // clean up
          }
      }

      updateSelection >> updateExpanded
    }

    val backButton = Reuse.always[VdomNode](
      Button(
        as = <.a,
        size = Mini,
        compact = true,
        basic = true,
        clazz = ExploreStyles.TileBackButton |+| ExploreStyles.BlendedButton,
        onClickE =
          linkOverride[ButtonProps](state.zoom(State.panelsSelected).set(SelectedPanel.tree))
      )(^.href := ctx.pageUrl(AppTab.Targets, none), Icons.ChevronLeft)
    )

    /**
     * Render the summary table.
     */
    def renderSummary: VdomNode =
      Tile("targetListSummary", "Target List Summary", backButton.some)(
        Reuse.by( // TODO Add reuseCurrying for higher arities in crystal
          (targetListGroupWithObs.get, props.hiddenColumns, props.focusedObs, props.expandedIds)
        )((renderInTitle: Tile.RenderInTitle) =>
          TargetSummaryTable(targetListGroupWithObs.get,
                             props.hiddenColumns,
                             state.zoom(State.panelsSelected),
                             props.focusedObs,
                             props.expandedIds,
                             renderInTitle
          ): VdomNode
        )
      )

    /**
     * Render the target env editor for a TargetEnv.
     *
     * @param idsToEdit
     *   The TargetEnvGroupIds to include in the edit. This needs to be a subset of the ids in
     *   targetListGroup
     * @param targetListGroup
     *   The TargetEnv that is the basis for editing. All or part of it may be included in the edit.
     */
    def renderEditor(idsToEdit: TargetEnvGroupIdSet, targetListGroup: TargetEnvGroup): VdomNode = {
      val focusedObs        = props.focusedObs.get
      val targetEnvGroupIds = targetListGroup.id

      val tlgView =
        targetListGroupWithObs
          .withOnMod(onModTargetsWithObs(targetEnvGroupIds, idsToEdit))
          .zoom(TargetListGroupWithObs.targetListGroups)

      val optFilteredEnv: Option[TargetEnvGroup] = if (idsToEdit =!= targetEnvGroupIds) {
        val targetIdsToEdit = targetListGroupWithObs.get.targetIdsFor(idsToEdit)
        targetListGroup.filterInIds(idsToEdit, targetIdsToEdit)
      } else targetListGroup.some

      optFilteredEnv.fold(renderSummary) { filteredEnv =>
        val getEnv: TargetListGroupList => TargetEnvGroup = _ => filteredEnv

        def modEnv(
          mod: TargetEnvGroup => TargetEnvGroup
        ): TargetListGroupList => TargetListGroupList =
          tlgl => {
            val moddedEnv = mod(filteredEnv)

            // if we're editing a subset of the group, we need to split the group up
            val splitList =
              if (idsToEdit === targetEnvGroupIds)
                tlgl.updated(targetEnvGroupIds, moddedEnv) // else just update the current
              else {
                val temp = tlgl - targetEnvGroupIds + moddedEnv.asObsKeyValue
                targetListGroup
                  .filterOutIds(filteredEnv.id, filteredEnv.targetIds)
                  .fold(temp)(env => temp + env.asObsKeyValue)
              }

            // see if the edit caused a merger - note that we are checking the original list for the duplicate
            val mergeWithTlg = tlgl.find(_._2.areScienceTargetsEqual(moddedEnv))
            mergeWithTlg.fold(splitList) { tlg =>
              splitList - tlg._1 - idsToEdit + tlg._2.merge(moddedEnv).asObsKeyValue
            }
          }

        val envView: View[TargetEnvGroup] = tlgView.zoom(getEnv)(modEnv)

        val title = focusedObs match {
          case Some(FocusedObs(id)) => s"Observation $id"
          case None                 =>
            val titleSfx = if (idsToEdit.size === 1) "" else "s"
            s"Editing ${idsToEdit.size} Target List$titleSfx"
        }

        Tile("targetEditor", title, backButton.some)(
          Reuse
            .by(
              (props.userId,
               envView,
               props.targetsUndoStacks,
               props.searching,
               state.zoom(State.options),
               props.hiddenColumns
              )
            )((renderInTitle: Tile.RenderInTitle) =>
              props.userId.map(uid =>
                <.div(
                  TargetEnvEditor(uid,
                                  envView,
                                  props.targetsUndoStacks,
                                  props.searching,
                                  state.zoom(State.options),
                                  props.hiddenColumns,
                                  renderInTitle
                  )
                )
              ): VdomNode
            )
            .reuseAlways
        )
      }
    }

    val coreWidth  = props.size.width.getOrElse(0) - treeWidth
    val coreHeight = props.size.height.getOrElse(0)

    val rightSide = state.get.panels.selected.optValue
      .flatMap(ids =>
        findTargetListGroup(ids, targetListGroupWithObs.get.targetListGroups).map(tlg => (ids, tlg))
      )
      .fold[VdomNode](renderSummary) { case (idsToEdit, targetListGroup) =>
        renderEditor(idsToEdit, targetListGroup)
      }

    // It would be nice to make a single component here but it gets hard when you
    // have the resizable element. Instead we have either two panels with a resizable
    // or only one panel at a time (Mobile)
    if (window.innerWidth <= Constants.TwoPanelCutoff) {
      <.div(
        ExploreStyles.TreeRGL,
        <.div(ExploreStyles.Tree, treeInner(targetListGroupWithObs))
          .when(state.get.panels.selected.leftPanelVisible),
        <.div(^.key := "target-right-side", ExploreStyles.SinglePanelTile)(
          rightSide
        ).when(state.get.panels.selected.rightPanelVisible)
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
      TargetListGroupLiveQuery(
        Reuse(renderFn _)(props, ViewF.fromState($))
      )
    }
  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .initialState(
        State(TwoPanelState.initial[TargetEnvGroupIdSet](SelectedPanel.Uninitialized),
              TargetVisualOptions.Default
        )
      )
      .renderBackend[Backend]
      .componentDidMount(readWidthPreference)
      .configure(Reusability.shouldComponentUpdate)
      .build

}
