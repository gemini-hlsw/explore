// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.Order._
import cats.effect.IO
import cats.syntax.all._
import crystal.ViewF
import crystal.react.View
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import explore.Icons
import explore.common.AsterismQueries._
import explore.common.UserPreferencesQueries._
import explore.common.UserPreferencesQueriesGQL._
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model._
import explore.model.enum.AppTab
import explore.model.reusability._
import explore.observationtree.AsterismGroupObsList
import explore.targeteditor.AsterismEditor
import explore.undo._
import japgolly.scalajs.react._
import japgolly.scalajs.react.component.builder.Lifecycle.ComponentDidMount
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.SiderealTarget
import lucuma.core.model.Target
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
  listUndoStacks:    View[UndoStacks[IO, AsterismGroupList]],
  targetsUndoStacks: View[Map[Target.Id, UndoStacks[IO, SiderealTarget]]],
  searching:         View[Set[Target.Id]],
  expandedIds:       View[SortedSet[ObsIdSet]],
  hiddenColumns:     View[Set[String]],
  size:              ResizeDetector.Dimensions
)(implicit val ctx:  AppContextIO)
    extends ReactProps[TargetTabContents](TargetTabContents.component)

object TargetTabContents {
  type Props = TargetTabContents

  final case class State(
    panels:  TwoPanelState[ObsIdSet],
    options: TargetVisualOptions // TODO: not setting fov from user preferences, yet
  )

  object State {
    val panels         = Focus[State](_.panels)
    val options        = Focus[State](_.options)
    val panelsWidth    = State.panels.andThen(TwoPanelState.treeWidth[ObsIdSet])
    val panelsSelected = State.panels.andThen(TwoPanelState.selected[ObsIdSet])
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive
  implicit val stateReuse: Reusability[State] = Reusability.derive

  val treeWidthLens = TwoPanelState.treeWidth[ObsIdSet]
  val selectedLens  = TwoPanelState.selected[ObsIdSet]

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
    props:                Props,
    state:                View[State],
    asterismGroupWithObs: View[AsterismGroupsWithObs]
  )(implicit ctx:         AppContextIO): VdomNode = {
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

    val targetMap = asterismGroupWithObs.get.targetGroups

    // Tree area
    def tree(objectsWithObs: View[AsterismGroupsWithObs]) =
      <.div(^.width := treeWidth.px, ExploreStyles.Tree |+| ExploreStyles.ResizableSinglePanel)(
        treeInner(objectsWithObs)
      )

    def treeInner(objectsWithObs: View[AsterismGroupsWithObs]) =
      <.div(ExploreStyles.TreeBody)(
        AsterismGroupObsList(
          objectsWithObs,
          props.focusedObs,
          state.zoom(State.panelsSelected),
          props.expandedIds,
          props.listUndoStacks
        )
      )

    def findAsterismGroup(
      obsIds: ObsIdSet,
      agl:    AsterismGroupList
    ): Option[AsterismGroup] = agl.values.find(_.obsIds.intersect(obsIds).nonEmpty)

    def onModAsterismsWithObs(
      groupIds:  ObsIdSet,
      editedIds: ObsIdSet
    )(tlgwo:     AsterismGroupsWithObs): Callback = {
      val groupList = tlgwo.asterismGroups

      // If we're editing at the group level (even a group of 1) and it no longer exists
      // (probably due to a merger), just go to the summary.
      val updateSelection = props.focusedObs.get match {
        case Some(_) => Callback.empty
        case _       =>
          groupList
            .get(editedIds)
            .fold(state.zoom(State.panelsSelected).set(SelectedPanel.summary))(_ => Callback.empty)
      }

      val updateExpanded = findAsterismGroup(editedIds, groupList).fold(Callback.empty) { tlg =>
        // We should always find the group.
        // If a group was edited while closed and it didn't create a merger, keep it closed,
        // otherwise expand all affected groups.
        props.expandedIds
          .mod { eids =>
            val withOld       =
              if (groupIds === editedIds) eids
              else eids + groupIds.removeUnsafe(editedIds)
            val withOldAndNew =
              if (editedIds === tlg.obsIds && editedIds === groupIds) withOld
              else withOld + tlg.obsIds

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
      Tile("asterismSummary", "Asterism Summary", backButton.some)(
        Reuse.by( // TODO Add reuseCurrying for higher arities in crystal
          (asterismGroupWithObs.get, props.hiddenColumns, props.focusedObs, props.expandedIds)
        )((_: Tile.RenderInTitle) => explore.UnderConstruction())
        // TODO: Fix the summary table and reinstate
        // )((renderInTitle: Tile.RenderInTitle) =>
        //   TargetSummaryTable(asterismGroupWithObs.get,
        //                      props.hiddenColumns,
        //                      state.zoom(State.panelsSelected),
        //                      props.focusedObs,
        //                      props.expandedIds,
        //                      renderInTitle
        //   ): VdomNode
        // )
      )

    /**
     * Render the asterism editor
     *
     * @param idsToEdit
     *   The observations to include in the edit. This needs to be a subset of the ids in
     *   asterismGroup
     * @param asterismGroup
     *   The AsterismGroup that is the basis for editing. All or part of it may be included in the
     *   edit.
     */
    def renderEditor(idsToEdit: ObsIdSet, asterismGroup: AsterismGroup): VdomNode = {
      val focusedObs = props.focusedObs.get
      val groupIds   = asterismGroup.obsIds
      val targetIds  = asterismGroup.targetIds

      val asterism = targetIds.toList.map(targetMap.get).flatten

      val getAsterism: AsterismGroupsWithObs => List[TargetWithId] = _ => asterism
      def modAsterism(
        mod: List[TargetWithId] => List[TargetWithId]
      ): AsterismGroupsWithObs => AsterismGroupsWithObs = agwo => {
        val asterismGroups      = agwo.asterismGroups
        val targetGroups        = agwo.targetGroups
        val moddedAsterism      = mod(asterism)
        val newTargetIds        = SortedSet.from(moddedAsterism.map(TargetWithId.id.get))
        val updatedTargetGroups = targetGroups ++ moddedAsterism.map(twi => (twi._1, twi))

        // if we're editing a subgroup, actions such as adding/removing a target would result in a split
        val splitAsterisms =
          if (idsToEdit === groupIds || targetIds === newTargetIds)
            asterismGroups
          else {
            asterismGroups - groupIds + asterismGroup
              .removeObsIdsUnsafe(idsToEdit)
              .asObsKeyValue + AsterismGroup(
              idsToEdit,
              newTargetIds
            ).asObsKeyValue
          }

        // see if the edit caused a merger.
        val oMergeWithAg = asterismGroups.find { case (obsIds, ag) =>
          obsIds =!= groupIds && ag.targetIds === newTargetIds
        }

        val updatedAsterismGroups = oMergeWithAg.fold(
          splitAsterisms.updated(idsToEdit, asterismGroup.copy(targetIds = newTargetIds))
        ) { mergeWithAg =>
          asterismGroups - idsToEdit + mergeWithAg._2.addObsIds(groupIds).asObsKeyValue
        }

        agwo.copy(asterismGroups = updatedAsterismGroups, targetGroups = updatedTargetGroups)
      }

      val asterismView: View[List[TargetWithId]] = asterismGroupWithObs
        .withOnMod(onModAsterismsWithObs(groupIds, idsToEdit))
        .zoom(getAsterism)(modAsterism)

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
             idsToEdit,
             asterismView,
             props.targetsUndoStacks,
             props.searching,
             state.zoom(State.options),
             props.hiddenColumns
            )
          )((renderInTitle: Tile.RenderInTitle) =>
            props.userId.map(uid =>
              <.div(
                AsterismEditor(uid,
                               idsToEdit,
                               asterismView,
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

    val coreWidth  = props.size.width.getOrElse(0) - treeWidth
    val coreHeight = props.size.height.getOrElse(0)

    val rightSide = state.get.panels.selected.optValue
      .flatMap(ids =>
        findAsterismGroup(ids, asterismGroupWithObs.get.asterismGroups).map(ag => (ids, ag))
      )
      .fold[VdomNode](renderSummary) { case (idsToEdit, asterismGroup) =>
        renderEditor(idsToEdit, asterismGroup)
      }

    // It would be nice to make a single component here but it gets hard when you
    // have the resizable element. Instead we have either two panels with a resizable
    // or only one panel at a time (Mobile)
    if (window.innerWidth <= Constants.TwoPanelCutoff) {
      <.div(
        ExploreStyles.TreeRGL,
        <.div(ExploreStyles.Tree, treeInner(asterismGroupWithObs))
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
          content = tree(asterismGroupWithObs),
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
      AsterismGroupLiveQuery(
        Reuse(renderFn _)(props, ViewF.fromState($))
      )
    }
  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .initialState(
        State(TwoPanelState.initial[ObsIdSet](SelectedPanel.Uninitialized),
              TargetVisualOptions.Default
        )
      )
      .renderBackend[Backend]
      .componentDidMount(readWidthPreference)
      .configure(Reusability.shouldComponentUpdate)
      .build

}
