// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.Order._
import cats.effect.IO
import cats.syntax.all._
import crystal.react.View
import crystal.react.hooks._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.string.NonEmptyString
import explore.Icons
import explore.common.AsterismQueries._
import explore.common.UserPreferencesQueries._
import explore.common.UserPreferencesQueriesGQL._
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model._
import explore.model.enum.AppTab
import explore.model.layout._
import explore.model.reusability._
import explore.observationtree.AsterismGroupObsList
import explore.targeteditor.AsterismEditor
import explore.undo._
import explore.syntax.ui._
import react.gridlayout._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.reusability._
import lucuma.ui.utils._
import org.scalajs.dom.window
import react.common._
import react.common.implicits._
import react.draggable.Axis
import react.resizable._
import react.resizeDetector._
import react.resizeDetector.hooks._
import react.semanticui.elements.button.Button
import react.semanticui.elements.button.Button.ButtonProps
import react.semanticui.sizes._

import scala.collection.immutable.SortedSet
import scala.concurrent.duration._
import explore.targeteditor.SkyPlotSection
import monocle.Optional
import explore.components.TileController

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
  val TargetId: NonEmptyString = "target"
  val PlotId: NonEmptyString   = "skyPlot"

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
                 i = TargetId.value
      ),
      LayoutItem(x = 0,
                 y = TargetMaxHeight.value,
                 w = DefaultWidth.value,
                 h = SkyPlotInitialHeightFraction,
                 minH = SkyPlotInitialHeightFraction,
                 i = PlotId.value
      )
    )
  )

  private val proportionalLayouts = defineStdLayouts(
    Map(
      (BreakpointName.lg, layoutLarge),
      (BreakpointName.md, layoutLarge)
    )
  )

  private def scaleLayout(l: Layout, h: Int): Layout =
    layoutItems.modify { l =>
      l.i match {
        case r if r === TargetId.value =>
          val height =
            (h * TargetIntialHeightFraction / TotalHeightFractions) / (Constants.GridRowHeight + 2*Constants.GridRowPadding)
          (layoutItemHeight
            .replace(height)
            .andThen(layoutItemMaxHeight.replace(2 * height))
            .andThen(layoutItemMinHeight.replace(height / 2)))(l)
        case r if r === PlotId.value   =>
          val height =
            (h * SkyPlotInitialHeightFraction / TotalHeightFractions) / (Constants.GridRowHeight + 2*Constants.GridRowPadding)
          (layoutItemHeight
            .replace(height)
            .andThen(layoutItemMaxHeight.replace(2 * height))
            .andThen(layoutItemMinHeight.replace(height / 2)))(l)
        case _                         => l
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

      val selectedTarget =
        asterismView.get.headOption.map(_.id).map { targetId =>
          val optional =
            Optional[List[TargetWithId], Target](_.find(_.id === targetId).map(_.target))(target =>
              _.map(twid => if (twid.id === targetId) TargetWithId(targetId, target) else twid)
            )

          asterismView.zoom(optional)
        }

      val targetEditorTile = Tile(TargetId, title, backButton.some, canMaximize = true)(
        Reuse
          .by(
            (props.userId,
             idsToEdit,
             asterismView,
             props.targetsUndoStacks,
             props.searching,
             options,
             props.hiddenColumns
            )
          )((renderInTitle: Tile.RenderInTitle) =>
            props.userId.map { uid =>
              <.div(
                AsterismEditor(uid,
                               idsToEdit,
                               asterismView,
                               props.targetsUndoStacks,
                               props.searching,
                               options,
                               props.hiddenColumns,
                               renderInTitle
                )
              )
            }: VdomNode
          )
          .reuseAlways
      )

      val testTile = Tile(TargetId, s"Test $focusedObs")(
        Reuse.by(focusedObs)(_ => <.div("ABC").reuseAlways)
      )

      val skyPlotTile =
        Tile(PlotId,
             "Elevation Plot",
             canMinimize = true,
             bodyClass = ExploreStyles.SkyPlotTileBody.some,
             tileClass = ExploreStyles.SkyPlotTile.some
        )(
          Reuse
            .by(
              (props.userId, coreWidth, coreHeight, selectedTarget)
            ) { (_: Tile.RenderInTitle) =>
              selectedTarget
                .flatMap(
                  _.mapValue(targetView =>
                    targetView.get match {
                      case t @ Target.Sidereal(_, _, _, _) =>
                        val baseCoordinates =
                          Target.Sidereal.baseCoordinates.get(t)
                        SkyPlotSection(baseCoordinates): VdomNode
                      case _                               => EmptyVdom
                    }
                  )
                )
                .getOrElse(EmptyVdom)
            }
            .reuseAlways
        )

      TileController(
        props.userId,
        coreWidth,
        null,
        layouts,
        List(testTile, skyPlotTile),
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

  var u = 0

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateView(TwoPanelState.initial[ObsIdSet](SelectedPanel.Uninitialized))
      .useStateView(TargetVisualOptions.Default)
      .useResizeDetector()
      .useStateView(proportionalLayouts)
      .useEffectWithDepsBy((_, _, _, r, _) => r.height) {
        (_, _, _, _, l) =>
          h =>
            h.map(h => l.mod(l => scaledLayout(h, l))).getOrEmpty
      }
      // .useEffectWithDepsBy((p, _, _, _, _) => p.focusedObs) { (props, panels, _, _, layout) =>
      //   implicit val ctx = props.ctx
      //   _ =>
      //     TabGridPreferencesQuery
      //       .queryWithDefault[IO](props.userId,
      //                             GridLayoutSection.TargetLayout,
      //                             ResizableSection.TargetsTree,
      //                             (Constants.InitialTreeWidth.toInt, defaultLayout)
      //       )
      //       .attempt
      //       .flatMap {
      //         case Right((w, l)) =>
      //           (panels
      //             .mod(
      //               TwoPanelState.treeWidth[ObsIdSet].replace(w)
      //             ) *> layout.set(mergeMap(layout.get, l)))
      //             .to[IO]
      //         case Left(_)       => IO.unit
      //       }
      //       .runAsync
      // }
      .useSingleEffect(debounce = 1.second)
      .render { (props, tps, opts, resize, layout, debouncer) =>
        implicit val ctx = props.ctx
        AsterismGroupLiveQuery(
          Reuse(renderFn _)(props, tps, opts, layout, resize, debouncer)
        )
      }

}
