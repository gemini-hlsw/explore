// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.Order._
import cats.effect.IO
import cats.syntax.all._
import crystal.Pot
import crystal.react.View
import crystal.react._
import crystal.react.hooks._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.NonNegInt
import explore.Icons
import explore.common.AsterismQueries._
import explore.common.UserPreferencesQueries._
import explore.common.UserPreferencesQueriesGQL._
import explore.components.Tile
import explore.components.TileController
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model._
import explore.model.enum.AppTab
import explore.model.layout._
import explore.model.layout.unsafe._
import explore.model.reusability._
import explore.observationtree.AsterismGroupObsList
import explore.syntax.ui._
import explore.undo._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.reusability._
import lucuma.ui.utils._
import monocle.Optional
import org.scalajs.dom.window
import react.common._
import react.common.implicits._
import react.draggable.Axis
import react.gridlayout._
import react.resizable._
import react.resizeDetector._
import react.resizeDetector.hooks._
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.Button.ButtonProps
import react.semanticui.sizes._

import scala.collection.immutable.SortedSet
import scala.concurrent.duration._

final case class TargetTabContents(
  userId:            Option[User.Id],
  focusedObs:        View[Option[FocusedObs]],
  listUndoStacks:    View[UndoStacks[IO, AsterismGroupList]],
  targetsUndoStacks: View[Map[Target.Id, UndoStacks[IO, Target.Sidereal]]],
  searching:         View[Set[Target.Id]],
  expandedIds:       View[SortedSet[ObsIdSet]],
  hiddenColumns:     View[Set[String]]
)(implicit val ctx:  AppContextIO)
    extends ReactFnProps[TargetTabContents](TargetTabContents.component)

object TargetTabContents {
  type Props = TargetTabContents

  private val TargetIntialHeightFraction   = 6
  private val SkyPlotInitialHeightFraction = 4
  private val TotalHeightFractions         = TargetIntialHeightFraction + SkyPlotInitialHeightFraction
  val TargetMaxHeight: NonNegInt           = 3
  val TargetMinHeight: NonNegInt           = 3
  val DefaultWidth: NonNegInt              = 12

  val layoutLarge: Layout = Layout(
    List(
      LayoutItem(x = 0,
                 y = 0,
                 w = DefaultWidth.value,
                 h = TargetIntialHeightFraction,
                 minH = TargetIntialHeightFraction,
                 i = ObsTabTiles.TargetId.value
      ),
      LayoutItem(x = 0,
                 y = TargetMaxHeight.value,
                 w = DefaultWidth.value,
                 h = SkyPlotInitialHeightFraction,
                 minH = SkyPlotInitialHeightFraction,
                 i = ObsTabTiles.PlotId.value
      )
    )
  )

  private val proportionalLayouts = defineStdLayouts(
    Map(
      (BreakpointName.lg, layoutLarge),
      (BreakpointName.md, layoutLarge)
    )
  )

  def layoutLens(height: Int) =
    layoutItemHeight
      .replace(height)
      .andThen(layoutItemMaxHeight.replace(2 * height))
      .andThen(layoutItemMinHeight.replace(height / 2))

  private def scaleLayout(l: Layout, h: Int): Layout =
    layoutItems.modify { l =>
      l.i match {
        case r if r === ObsTabTiles.TargetId.value =>
          val height =
            (h * TargetIntialHeightFraction / TotalHeightFractions) / (Constants.GridRowHeight + Constants.GridRowPadding)
          layoutLens(height)(l)
        case r if r === ObsTabTiles.PlotId.value   =>
          val height =
            (h * SkyPlotInitialHeightFraction / TotalHeightFractions) / (Constants.GridRowHeight + Constants.GridRowPadding)
          layoutLens(height)(l)
        case _                                     => l
      }
    }(l)

  private def scaledLayout(h: Int, l: LayoutsMap): LayoutsMap =
    l.map { case (k, (a, b, l)) =>
      (k, (a, b, scaleLayout(l, h)))
    }

  implicit val propsReuse: Reusability[Props] = Reusability.derive

  val treeWidthLens = TwoPanelState.treeWidth[ObsIdSet]
  val selectedLens  = TwoPanelState.selected[ObsIdSet]

  protected def renderFn(
    props:                Props,
    panels:               View[TwoPanelState[ObsIdSet]],
    options:              View[TargetVisualOptions],
    defaultLayouts:       LayoutsMap,
    layouts:              View[LayoutsMap],
    resize:               UseResizeDetectorReturn,
    debouncer:            Reusable[UseSingleEffect[IO]],
    asterismGroupWithObs: View[AsterismGroupsWithObs]
  )(implicit ctx:         AppContextIO): VdomNode = {
    val panelsResize =
      (_: ReactEvent, d: ResizeCallbackData) =>
        panels.zoom(treeWidthLens).set(d.size.width) *>
          debouncer
            .submit(
              UserWidthsCreation
                .storeWidthPreference[IO](props.userId, ResizableSection.TargetsTree, d.size.width)
            )
            .runAsyncAndForget

    val treeWidth    = panels.get.treeWidth.toInt
    val selectedView = panels.zoom(selectedLens)

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
          selectedView,
          props.expandedIds,
          props.listUndoStacks
        )
      )

    def findAsterismGroup(
      obsIds: ObsIdSet,
      agl:    AsterismGroupList
    ): Option[AsterismGroup] = agl.values.find(_.obsIds.intersects(obsIds))

    def onModAsterismsWithObs(
      groupIds:  ObsIdSet,
      editedIds: ObsIdSet
    )(agwo:      AsterismGroupsWithObs): Callback =
      findAsterismGroup(editedIds, agwo.asterismGroups).foldMap { tlg =>
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

            withOldAndNew.filter(ids => agwo.asterismGroups.contains(ids)) // clean up
          }
      }

    val backButton = Reuse.always[VdomNode](
      Button(
        as = <.a,
        size = Mini,
        compact = true,
        basic = true,
        clazz = ExploreStyles.TileBackButton |+| ExploreStyles.BlendedButton,
        onClickE = linkOverride[ButtonProps](
          selectedView.set(SelectedPanel.tree)
        )
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
        //                      selectedView,
        //                      props.focusedObs,
        //                      props.expandedIds,
        //                      renderInTitle
        //   ): VdomNode
        // )
      )

    val coreWidth  = resize.width.getOrElse(0) - treeWidth
    val coreHeight = resize.height.getOrElse(0)

    /**
     * Render the asterism editor
     *
     * @param idsToEdit
     *   The observations to include in the edit. This needs to be asubset of the ids in
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
        val newTargetIds        = SortedSet.from(moddedAsterism.map(_.id))
        // make sure any added targets are in the map and update modified ones.
        val updatedTargetGroups = targetGroups ++ moddedAsterism.map(twi => (twi.id, twi))

        val splitAsterisms =
          if (targetIds === newTargetIds)
            asterismGroups
          else if (idsToEdit === groupIds) {
            asterismGroups + asterismGroup.copy(targetIds = newTargetIds).asObsKeyValue
          } else {
            // Since we're editing a subgroup, actions such as adding/removing a target will result in a split
            asterismGroups - groupIds +
              asterismGroup.removeObsIdsUnsafe(idsToEdit).asObsKeyValue +
              AsterismGroup(idsToEdit, newTargetIds).asObsKeyValue
          }

        // see if the edit caused a merger - note that we're searching the original lists.
        val oMergeWithAg = asterismGroups.find { case (obsIds, ag) =>
          obsIds =!= groupIds && ag.targetIds === newTargetIds
        }

        val updatedAsterismGroups = oMergeWithAg.fold(
          splitAsterisms
        ) { mergeWithAg =>
          splitAsterisms - idsToEdit - mergeWithAg._1 +
            mergeWithAg._2.addObsIds(groupIds).asObsKeyValue
        }

        agwo.copy(asterismGroups = updatedAsterismGroups, targetGroups = updatedTargetGroups)
      }

      val asterismView: View[List[TargetWithId]] = asterismGroupWithObs
        .withOnMod(onModAsterismsWithObs(groupIds, idsToEdit))
        .zoom(getAsterism)(modAsterism)

      val title = focusedObs match {
        case Some(FocusedObs(id)) => s"Observation $id"
        case None                 =>
          s"Editing ${idsToEdit.size} Asterisms"
      }

      val selectedTarget: Option[ViewOpt[Target]] =
        asterismView.get.headOption.map(_.id).map { targetId =>
          val optional =
            Optional[List[TargetWithId], Target](_.find(_.id === targetId).map(_.target))(target =>
              _.map(twid => if (twid.id === targetId) TargetWithId(targetId, target) else twid)
            )

          asterismView.zoom(optional)
        }

      val targetEditorTile =
        TargetTile.targetTile(
          props.userId,
          idsToEdit,
          Pot(asterismView),
          props.targetsUndoStacks,
          props.searching,
          options,
          title,
          backButton.some,
          props.hiddenColumns,
          coreWidth,
          coreHeight
        )

      val selectedCoordinates = selectedTarget
        .flatMap(
          _.mapValue(targetView =>
            targetView.get match {
              case t @ Target.Sidereal(_, _, _, _) =>
                Target.Sidereal.baseCoordinates.get(t).some
              case _                               => none
            }
          )
        )

      val skyPlotTile =
        selectedCoordinates.flatten.map(
          ElevationPlotTile.elevationPlotTile(coreWidth, coreHeight, _)
        )

      TileController(
        props.userId,
        coreWidth,
        defaultLayouts,
        layouts,
        List(targetEditorTile.some, skyPlotTile).collect { case Some(x) => x },
        GridLayoutSection.TargetLayout,
        None
      )
    }

    val selectedPanel = panels.get.selected
    val rightSide     = selectedPanel.optValue
      .flatMap(ids =>
        findAsterismGroup(ids, asterismGroupWithObs.get.asterismGroups).map(ag => (ids, ag))
      )
      .fold[VdomNode](renderSummary) { case (idsToEdit, asterismGroup) =>
        renderEditor(idsToEdit, asterismGroup)
      }

    // It would be nice to make a single component here but it gets hard when you
    // have the resizable element. Instead we have either two panels with a resizable
    // or only one panel at a time (Mobile)
    val body = if (window.canFitTwoPanels) {
      <.div(
        ExploreStyles.TreeRGL,
        <.div(ExploreStyles.Tree, treeInner(asterismGroupWithObs))
          .when(selectedPanel.leftPanelVisible),
        <.div(^.key := "target-right-side", ExploreStyles.SinglePanelTile)(
          rightSide
        ).when(selectedPanel.rightPanelVisible)
      )
    } else {
      <.div(
        ExploreStyles.TreeRGL,
        Resizable(
          axis = Axis.X,
          width = treeWidth,
          height = coreHeight,
          minConstraints = (Constants.MinLeftPanelWidth.toInt, 0),
          maxConstraints = (coreWidth / 2, 0),
          onResize = panelsResize,
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
    body.withRef(resize.ref)
  }

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateView(TwoPanelState.initial[ObsIdSet](SelectedPanel.Uninitialized))
      .useStateView(TargetVisualOptions.Default)
      .useResizeDetector()
      .useStateView(proportionalLayouts)
      .useMemoBy((_, _, _, r, _) => r.height) { (_, _, _, _, l) => h =>
        // Memoize the initial result
        h.map(h => scaledLayout(h, l.get)).getOrElse(l.get)
      }
      .useEffectWithDepsBy((p, _, _, r, _, _) => (p.userId, r.height)) {
        (props, panels, _, _, layout, defaultLayout) =>
          implicit val ctx = props.ctx
          (params: (Option[User.Id], Option[Int])) => {
            val (u, h) = params
            TabGridPreferencesQuery
              .queryWithDefault[IO](u,
                                    GridLayoutSection.TargetLayout,
                                    ResizableSection.TargetsTree,
                                    (Constants.InitialTreeWidth.toInt, defaultLayout)
              )
              .attempt
              .flatMap {
                case Right((w, l)) =>
                  (panels
                    .mod(
                      TwoPanelState.treeWidth[ObsIdSet].replace(w)
                    ) *> layout.mod(o => mergeMap(o, l)))
                    .to[IO]
                case Left(_)       =>
                  h.map(h => layout.mod(l => scaledLayout(h, l)).to[IO]).getOrElse(IO.unit)
              }
          }
      }
      .useSingleEffect(debounce = 1.second)
      .renderWithReuse { (props, tps, opts, resize, layout, defaultLayout, debouncer) =>
        implicit val ctx = props.ctx
        AsterismGroupLiveQuery(
          Reuse(renderFn _)(props, tps, opts, defaultLayout, layout, resize, debouncer)
        )
      }

}
