// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.data.NonEmptySet
import cats.effect.IO
import cats.effect.SyncIO
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
import explore.targeteditor.TargetEnvEditor
import explore.observationtree.TargetListGroupObsList
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
import scala.collection.immutable.TreeSeqMap
import scala.concurrent.duration._

final case class TargetTabContents(
  userId:            Option[User.Id],
  focusedObs:        View[Option[FocusedObs]],
  listUndoStacks:    View[UndoStacks[IO, TargetListGroupList]],
  targetsUndoStacks: View[Map[TargetIdSet, UndoStacks[IO, SiderealTarget]]],
  searching:         View[Set[TargetIdSet]],
  expandedIds:       View[SortedSet[TargetEnvIdObsIdSet]],
  hiddenColumns:     View[Set[String]],
  size:              ResizeDetector.Dimensions
)(implicit val ctx:  AppContextIO)
    extends ReactProps[TargetTabContents](TargetTabContents.component)

object TargetTabContents {
  type Props = TargetTabContents

  final case class State(
    panels:  TwoPanelState[TargetEnvIdObsIdSet],
    options: TargetVisualOptions // TODO: not setting fov from user preferences, yet
  )

  object State {
    val panels         = Focus[State](_.panels)
    val options        = Focus[State](_.options)
    val panelsWidth    = State.panels.andThen(TwoPanelState.treeWidth[TargetEnvIdObsIdSet])
    val panelsSelected = State.panels.andThen(TwoPanelState.selected[TargetEnvIdObsIdSet])
  }

  implicit val propsReuse: Reusability[Props] = Reusability.derive
  implicit val stateReuse: Reusability[State] = Reusability.derive

  val treeWidthLens = TwoPanelState.treeWidth[TargetEnvIdObsIdSet]
  val selectedLens  = TwoPanelState.selected[TargetEnvIdObsIdSet]

  def readWidthPreference($ : ComponentDidMount[Props, State, _]): Callback = {
    implicit val ctx = $.props.ctx
    UserAreaWidths
      .queryWithDefault[IO]($.props.userId,
                            ResizableSection.TargetsTree,
                            Constants.InitialTreeWidth.toInt
      )
      .runAsyncAndThenCB {
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
            )).runAsyncCB
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
      targetEnvIds: TargetEnvIdObsIdSet,
      tlgl:         TargetListGroupList
    ): Option[TargetEnv] = tlgl.values.find(_.id.intersect(targetEnvIds).nonEmpty)

    def onModTargetsWithObs(
      groupIds:  TargetEnvIdObsIdSet,
      editedIds: TargetEnvIdObsIdSet
    )(tlgwo:     TargetListGroupWithObs): SyncIO[Unit] = {
      val groupList = tlgwo.targetListGroups

      // If we're editing at the group level (even a group of 1) and it no longer exists
      // (probably due to a merger), just go to the summary.
      val updateSelection = props.focusedObs.get match {
        case Some(_) => SyncIO.unit
        case _       =>
          groupList
            .get(editedIds)
            .fold(state.zoom(State.panelsSelected).set(SelectedPanel.summary))(_ => SyncIO.unit)
      }

      val updateExpanded = findTargetListGroup(editedIds, groupList).fold(SyncIO.unit) { tlg =>
        // We should always find the group.
        // If a group was edited while closed and it didn't create a merger, keep it closed,
        // otherwise expand all affected groups.
        props.expandedIds
          .mod { eids =>
            val withOld       =
              if (groupIds === editedIds) eids
              else eids + NonEmptySet.fromSetUnsafe(groupIds -- editedIds)
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
     * Effectively creates a subset of the original TargetEnv, where the TargetEnv.id contains only
     * ids in envObsIds, and the keys for the TargetEnv.scienceTargets map contain only the keys
     * specified in targetIds. This subset can then be passed to the TargetEnvEditor.
     */
    def filterInIds(
      envObsIds: TargetEnvIdObsIdSet,
      targetIds: Set[Target.Id],
      original:  TargetEnv
    ): Option[TargetEnv] = {
      val filteredIds: Option[TargetEnvIdObsIdSet] =
        NonEmptySet.fromSet(original.id.filter(envObsIds.contains))

      val filteredTargets: Option[TreeSeqMap[TargetIdSet, Target]] =
        original.scienceTargets.toList
          .traverse { case (ids, target) =>
            NonEmptySet
              .fromSet(ids.filter(targetIds.contains))
              .map((_, target))
          }
          .map(TreeSeqMap.from)
      (filteredIds, filteredTargets).mapN { case (id, targets) => TargetEnv(id, targets) }
    }

    /**
     * Creates a subset of the original TargetEnv where the TargetEnv.id has had all of the ids in
     * toFilterOut.id removed, and the TargetEnv.scienceTarget keys have had any keys in
     * toFilter.scienceTargets removed. This is used to create a new TargetEnv when a subset of the
     * original has been edited, necessitating a split. NOTE: toFilterOut should be a subset of
     * original with identical targets.
     */
    def filterOutIds(original: TargetEnv, toFilterOut: TargetEnv): Option[TargetEnv] = {
      val filteredIds: Option[TargetEnvIdObsIdSet]                 =
        NonEmptySet.fromSet(original.id -- toFilterOut.id)
      // TODO: Validate the scienceTarget collections have the same targets?
      val filteredTargets: Option[TreeSeqMap[TargetIdSet, Target]] =
        (original.scienceTargets.toList, toFilterOut.scienceTargets.toList)
          .parMapN { case ((allIds, target), (outIds, _)) =>
            NonEmptySet.fromSet(allIds -- outIds).map((_, target))
          }
          .sequence
          .map(TreeSeqMap.from)
      (filteredIds, filteredTargets).mapN { case (id, targets) => TargetEnv(id, targets) }
    }

    /**
     * Given 2 TargetEnvs, this creates a new one where TargetEnv.id is a union of the env1.id and
     * env2.id. Each of the keys for the targets in TargetEnv.scienceTargets will be the union of
     * the keys in the respective targets in env1 and env2. This is used to merge 2 TargetEnvs in
     * the instance where editing a TargetEnv makes it equal to an existing TargetEnv, making a
     * merger necessary, NOTE: The target lists of env1 and env2 are assumed to be equal.
     */
    def mergeTargetEnvs(env1: TargetEnv, env2: TargetEnv): TargetEnv = {
      val mergedTargets: TreeSeqMap[TargetIdSet, Target] =
        TreeSeqMap.from(
          (env1.scienceTargets.toList, env2.scienceTargets.toList)
            .parMapN { case ((ids1, target), (ids2, _)) => (ids1 ++ ids2, target) }
        )
      TargetEnv(env1.id ++ env2.id, mergedTargets)
    }

    /**
     * Compare the targets in the scienceTargets of 2 TargetEnvs to see if they are all equal. This
     * is used to determine if a merger is necessary after an edit.
     */
    def compareTargets(env1: TargetEnv, env2: TargetEnv): Boolean =
      env1.scienceTargets.size === env2.scienceTargets.size &&
        (env1.scienceTargets.toList, env2.scienceTargets.toList)
          .parMapN { case (t1, t2) => t1._2 === t2._2 }
          .forall(identity)

    /**
     * Render the summary table.
     */
    def renderSummary: VdomNode =
      Tile("targetListSummary", "Target List Summary", backButton.some)(
        ((_: Tile.RenderInTitle) => <.div("Summary will go here")).reuseAlways
      )

    /**
     * Render the target env editor for a TargetEnv.
     *
     * @param idsToEdit
     *   The TargetEnvIdObsIds to include in the edit. This needs to be a subset of the ids in
     *   targetListGroup
     * @param targetListGroup
     *   The TargetEnv that is the basis for editing. All or part of it may be included in the edit.
     */
    def renderEditor(idsToEdit: TargetEnvIdObsIdSet, targetListGroup: TargetEnv): VdomNode = {
      val focusedObs   = props.focusedObs.get
      val groupEnvIds  = targetListGroup.id
      val observations = targetListGroupWithObs.get.observations

      val tlgView =
        targetListGroupWithObs
          .withOnMod(onModTargetsWithObs(groupEnvIds, idsToEdit))
          .zoom(TargetListGroupWithObs.targetListGroups) // will need withOnMod

      val optFilteredEnv: Option[TargetEnv] = if (idsToEdit =!= groupEnvIds) {
        val targetIdsToEdit = observations.mapFilter { obsSumm =>
          if (idsToEdit.contains((obsSumm.targetEnvId, obsSumm.id.some)))
            obsSumm.scienceTargetIds.some
          else none
        }.combineAll
        filterInIds(idsToEdit, targetIdsToEdit, targetListGroup)
      } else targetListGroup.some

      optFilteredEnv.fold(renderSummary) { filteredEnv =>
        val getEnv: TargetListGroupList => TargetEnv = _ => filteredEnv

        def modEnv(mod: TargetEnv => TargetEnv): TargetListGroupList => TargetListGroupList =
          tlgl => {
            val moddedEnv = mod(filteredEnv)

            // if we're editing a subset of the group, we need to split the group up
            val splitList =
              if (idsToEdit === groupEnvIds)
                tlgl.updated(groupEnvIds, moddedEnv) // else just update the current
              else {
                val temp = tlgl - groupEnvIds + moddedEnv.asObsKeyValue
                filterOutIds(targetListGroup, filteredEnv).fold(temp)(env =>
                  temp + env.asObsKeyValue
                )
              }

            // see if the edit caused a merger
            val mergeWithTlg = tlgl.find(tlg => compareTargets(tlg._2, moddedEnv))
            mergeWithTlg.fold(splitList) { tlg =>
              splitList - tlg._1 - idsToEdit + mergeTargetEnvs(tlg._2, moddedEnv).asObsKeyValue
            }
          }

        val envView: View[TargetEnv] = tlgView.zoom(getEnv)(modEnv)

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
        Reuse(renderFn _)(props, ViewF.fromStateSyncIO($))
      )
    }
  }

  protected val component =
    ScalaComponent
      .builder[Props]
      .initialState(
        State(TwoPanelState.initial[TargetEnvIdObsIdSet](SelectedPanel.Uninitialized),
              TargetVisualOptions.Default
        )
      )
      .renderBackend[Backend]
      .componentDidMount(readWidthPreference)
      .configure(Reusability.shouldComponentUpdate)
      .build

}
