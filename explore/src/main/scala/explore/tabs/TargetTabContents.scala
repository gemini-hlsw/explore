// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.Order._
import cats.effect.IO
import cats.syntax.all._
import crystal.Pot
import crystal.react._
import crystal.react.hooks._
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.auto._
import eu.timepit.refined.types.numeric.NonNegInt
import explore.Icons
import explore.common.AsterismQueries._
import explore.common.UserPreferencesQueries._
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
import explore.optics._
import explore.syntax.ui._
import explore.targets.TargetSummaryTable
import explore.undo._
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.Target.Nonsidereal
import lucuma.core.model.Target.Sidereal
import lucuma.core.model.User
import lucuma.ui.reusability._
import lucuma.ui.utils._
import monocle.Optional
import org.scalajs.dom.window
import queries.common.UserPreferencesQueriesGQL._
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

import java.time.LocalDateTime
import scala.collection.immutable.SortedSet
import scala.concurrent.duration._

final case class TargetTabContents(
  userId:            Option[User.Id],
  programId:         Program.Id,
  focusedObsSet:     Option[ObsIdSet],
  focusedTarget:     Option[Target.Id],
  listUndoStacks:    ReuseView[UndoStacks[IO, AsterismGroupsWithObs]],
  targetsUndoStacks: ReuseView[Map[Target.Id, UndoStacks[IO, Target.Sidereal]]],
  searching:         ReuseView[Set[Target.Id]],
  expandedIds:       ReuseView[SortedSet[ObsIdSet]],
  hiddenColumns:     ReuseView[Set[String]]
)(implicit val ctx:  AppContextIO)
    extends ReactFnProps[TargetTabContents](TargetTabContents.component)

object TargetTabContents {
  type Props = TargetTabContents

  private val TargetHeight: NonNegInt      = 18
  private val TargetMinHeight: NonNegInt   = 15
  private val SkyPlotHeight: NonNegInt     = 9
  private val SkyPlotMinHeight: NonNegInt  = 6
  private val TileMinWidth: NonNegInt      = 5
  private val DefaultWidth: NonNegInt      = 10
  private val DefaultLargeWidth: NonNegInt = 12

  val layoutLarge: Layout = Layout(
    List(
      LayoutItem(i = ObsTabTiles.TargetId.value,
                 x = 0,
                 y = 0,
                 w = DefaultWidth.value,
                 h = TargetHeight.value,
                 minH = TargetMinHeight.value,
                 minW = TileMinWidth.value
      ),
      LayoutItem(
        i = ObsTabTiles.PlotId.value,
        x = 0,
        y = TargetHeight.value,
        w = DefaultWidth.value,
        h = SkyPlotHeight.value,
        minH = SkyPlotMinHeight.value,
        minW = TileMinWidth.value
      )
    )
  )

  private val defaultTargetLayouts = defineStdLayouts(
    Map(
      (BreakpointName.lg,
       layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(layoutLarge)
      ),
      (BreakpointName.md, layoutLarge)
    )
  )

  implicit val propsReuse: Reusability[Props] = Reusability.derive

  def otherObsCount(
    targetGroupMap: Reuse[TargetGroupList],
    obsIds:         ObsIdSet,
    targetId:       Target.Id
  ): Int =
    targetGroupMap.get(targetId).fold(0)(tg => (tg.obsIds -- obsIds.toSortedSet).size)

  protected def renderFn(
    props:                 Props,
    panels:                ReuseView[TwoPanelState],
    defaultLayouts:        LayoutsMap,
    layouts:               ReuseView[LayoutsMap],
    resize:                UseResizeDetectorReturn,
    debouncer:             Reusable[UseSingleEffect[IO]],
    asterismGroupsWithObs: ReuseView[AsterismGroupsWithObs]
  )(implicit ctx:          AppContextIO): VdomNode = {

    val panelsResize =
      (_: ReactEvent, d: ResizeCallbackData) =>
        panels.zoom(TwoPanelState.treeWidth).set(d.size.width.toDouble) *>
          debouncer
            .submit(
              UserWidthsCreation
                .storeWidthPreference[IO](props.userId, ResizableSection.TargetsTree, d.size.width)
            )
            .runAsyncAndForget

    val treeWidth: Int = panels.get.treeWidth.toInt

    val selectedView: ReuseView[SelectedPanel] = panels.zoom(TwoPanelState.selected)

    val targetMap: Reuse[TargetGroupList] = asterismGroupsWithObs.map(_.get.targetGroups)

    // Tree area
    def tree(objectsWithObs: ReuseView[AsterismGroupsWithObs]) =
      <.div(^.width := treeWidth.px, ExploreStyles.Tree |+| ExploreStyles.ResizableSinglePanel)(
        treeInner(objectsWithObs)
      )

    def treeInner(objectsWithObs: ReuseView[AsterismGroupsWithObs]) =
      <.div(ExploreStyles.TreeBody)(
        AsterismGroupObsList(
          objectsWithObs,
          props.programId,
          props.focusedObsSet,
          props.focusedTarget,
          selectedView.set(SelectedPanel.summary).reuseAlways,
          props.expandedIds,
          props.listUndoStacks
        )
      )

    def findAsterismGroup(
      obsIds: ObsIdSet,
      agl:    AsterismGroupList
    ): Option[AsterismGroup] =
      agl.values.find(ag => obsIds.subsetOf(ag.obsIds))

    def setPage(obsIds: Option[ObsIdSet], targetId: Option[Target.Id]): Callback =
      props.ctx.pushPage(AppTab.Targets, props.programId, obsIds, targetId)

    def selectObservationAndTarget(
      expandedIds: ReuseView[SortedSet[ObsIdSet]],
      obsId:       Observation.Id,
      targetId:    Target.Id
    ): Callback = {
      val obsIdSet = ObsIdSet.one(obsId)
      findAsterismGroup(obsIdSet, asterismGroupsWithObs.get.asterismGroups)
        .map(ag => expandedIds.mod(_ + ag.obsIds))
        .orEmpty >>
        setPage(obsIdSet.some, targetId.some)
    }

    def selectTarget(targetId: Target.Id): Callback = setPage(none, targetId.some)

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
          ctx.pushPage(AppTab.Targets, props.programId, none, none) >>
            selectedView.set(SelectedPanel.tree)
        )
      )(^.href := ctx.pageUrl(AppTab.Targets, props.programId, none, none), Icons.ChevronLeft)
    )

    /**
     * Render the summary table.
     */
    def renderSummary: VdomNode =
      Tile("targetSummary", "Target Summary", backButton.some)(
        Reuse
          .currying(
            targetMap,
            props.hiddenColumns,
            props.expandedIds
          )
          .in((targets, hiddenColumns, expandedIds) =>
            (renderInTitle: Tile.RenderInTitle) =>
              TargetSummaryTable(
                targets,
                hiddenColumns,
                Reuse(selectObservationAndTarget _)(
                  expandedIds
                ),
                (selectTarget _).reuseAlways,
                renderInTitle
              ): VdomNode
          )
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
    def renderAsterismEditor(idsToEdit: ObsIdSet, asterismGroup: AsterismGroup): VdomNode = {
      val groupIds  = asterismGroup.obsIds
      val targetIds = asterismGroup.targetIds

      val asterism: List[TargetWithId] =
        targetIds.toList.map(id => targetMap.get(id).map(_.targetWithId)).flatten

      val getAsterism: AsterismGroupsWithObs => List[TargetWithId] = _ => asterism

      def modAsterism(
        mod: List[TargetWithId] => List[TargetWithId]
      ): AsterismGroupsWithObs => AsterismGroupsWithObs = agwo => {
        val asterismGroups = agwo.asterismGroups
        val targetGroups   = agwo.targetGroups
        val moddedAsterism = mod(asterism)
        val newTargetIds   = SortedSet.from(moddedAsterism.map(_.id))

        // make sure any added targets are in the map and update modified ones.
        val addedIds  = newTargetIds -- targetIds
        val tgUpdate1 =
          moddedAsterism.foldRight(targetGroups) { case (twid, groups) =>
            if (addedIds.contains(twid.id))
              // it's new to this asterism, but the target itself may or may not be new. So we
              // either add a new target group or update the existing one.
              groups.updatedWith(twid.id)(
                _.map(_.addObsIds(idsToEdit).copy(targetWithId = twid))
                  .orElse(TargetGroup(idsToEdit.toSortedSet, twid).some)
              )
            else // just update the current target, observations should be the same
              groups.updatedWith(twid.id)(_.map(_.copy(targetWithId = twid)))
          }

        val removedIds          = targetIds -- newTargetIds
        // If we removed a target, just update the observation ids for that target group
        val updatedTargetGroups = removedIds.foldRight(tgUpdate1) { case (id, groups) =>
          groups.updatedWith(id)(_.map(_.removeObsIds(idsToEdit)))
        }

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

      val asterismView: ReuseView[List[TargetWithId]] =
        asterismGroupsWithObs
          .map(
            _.withOnMod(onModAsterismsWithObs(groupIds, idsToEdit))
              .zoom(getAsterism)(modAsterism)
          )
          .addReuseByFrom(panels)

      val title = idsToEdit.single match {
        case Some(id) => s"Observation $id"
        case None     => s"Editing ${idsToEdit.size} Asterisms"
      }

      val selectedTarget: Option[ReuseViewOpt[Target]] =
        props.focusedTarget.map { targetId =>
          val optional =
            Optional[List[TargetWithId], Target](_.find(_.id === targetId).map(_.target))(target =>
              _.map(twid => if (twid.id === targetId) TargetWithId(targetId, target) else twid)
            )

          asterismView.zoom(optional).addReuseBy(targetId)
        }

      val scienceMode = idsToEdit.single match {
        case Some(id) =>
          asterismGroupsWithObs
            .zoom(AsterismGroupsWithObs.observations)
            .get
            .collect {
              case (k, ObsSummaryWithConstraintsAndConf(_, _, _, _, _, _, Some(v))) if k === id => v
            }
            .headOption
        case _        => None
      }

      // Until these are in the API
      val obsConfiguration =
        scienceMode.map(_ => ObsConfiguration(PosAngle.Default, LocalDateTime.now()))

      def setCurrentTarget(
        programId: Program.Id,
        oids:      ObsIdSet,
        tid:       Option[Target.Id],
        via:       SetRouteVia
      ): Callback =
        ctx.setPageVia(AppTab.Targets, programId, oids.some, tid, via)

      val asterismEditorTile =
        AsterismEditorTile.asterismEditorTile(
          props.userId,
          props.programId,
          idsToEdit,
          Pot(asterismView, scienceMode),
          obsConfiguration,
          props.focusedTarget,
          Reuse(setCurrentTarget _)(props.programId, idsToEdit),
          Reuse.currying(targetMap, idsToEdit).in(otherObsCount _),
          props.targetsUndoStacks,
          props.searching,
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
        ElevationPlotTile.elevationPlotTile(coreWidth, coreHeight, selectedCoordinates.flatten)

      TileController(
        props.userId,
        coreWidth,
        defaultLayouts,
        layouts,
        List(asterismEditorTile, skyPlotTile),
        GridLayoutSection.TargetLayout,
        None
      )
    }

    def renderSiderealTargetEditor(targetId: Target.Id, target: Target.Sidereal): VdomNode = {
      val getTarget: TargetGroupList => Target.Sidereal                                          = _ => target
      def modTarget(mod: Target.Sidereal => Target.Sidereal): TargetGroupList => TargetGroupList =
        _.updatedWith(targetId)(
          _.map(TargetGroup.targetWithId.replace(TargetWithId(targetId, mod(target))))
        )

      val targetView: ReuseView[Target.Sidereal] =
        asterismGroupsWithObs.map(
          _.zoom(AsterismGroupsWithObs.targetGroups).zoom(getTarget)(modTarget)
        )

      val title = s"Editing Target ${target.name.value} [$targetId]"

      val targetTile = SiderealTargetEditorTile.sideralTargetEditorTile(
        props.userId,
        targetId,
        targetView,
        props.targetsUndoStacks.zoom(atMapWithDefault(targetId, UndoStacks.empty)),
        props.searching,
        title,
        backButton.some,
        coreWidth,
        coreHeight
      )

      val skyPlotTile =
        ElevationPlotTile.elevationPlotTile(coreWidth,
                                            coreHeight,
                                            Target.Sidereal.baseCoordinates.get(target).some
        )

      TileController(props.userId,
                     coreWidth,
                     defaultLayouts,
                     layouts,
                     List(targetTile, skyPlotTile),
                     GridLayoutSection.TargetLayout,
                     None
      )
    }

    val selectedPanel = panels.get.selected
    val optSelected   = (props.focusedObsSet, props.focusedTarget) match {
      case (Some(obsIdSet), _)    => obsIdSet.asRight.some
      case (None, Some(targetId)) => targetId.asLeft.some
      case _                      => none
    }

    val rightSide: VdomNode =
      optSelected
        .fold(renderSummary) {
          _ match {
            case Left(targetId) =>
              targetMap
                .get(targetId)
                .fold(renderSummary)(u =>
                  u.targetWithId.target match {
                    case Nonsidereal(_, _, _)     =>
                      <.div("Editing of Non-Sidereal targets not supported")
                    case s @ Sidereal(_, _, _, _) =>
                      renderSiderealTargetEditor(targetId, s)
                  }
                )
            case Right(obsIds)  =>
              findAsterismGroup(obsIds, asterismGroupsWithObs.get.asterismGroups)
                .fold(renderSummary) { asterismGroup =>
                  renderAsterismEditor(obsIds, asterismGroup)
                }
          }
        }

    // It would be nice to make a single component here but it gets hard when you
    // have the resizable element. Instead we have either two panels with a resizable
    // or only one panel at a time (Mobile)
    if (window.canFitTwoPanels) {
      <.div(
        ExploreStyles.TreeRGL,
        <.div(ExploreStyles.Tree, treeInner(asterismGroupsWithObs))
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
          width = treeWidth.toDouble,
          height = coreHeight.toDouble,
          minConstraints = (Constants.MinLeftPanelWidth.toInt, 0),
          maxConstraints = (coreWidth / 2, 0),
          onResize = panelsResize,
          resizeHandles = List(ResizeHandleAxis.East),
          content = tree(asterismGroupsWithObs),
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

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      // Two panel state
      .useStateViewWithReuse(TwoPanelState.initial(SelectedPanel.Uninitialized))
      .useEffectWithDepsBy((props, state) =>
        (props.focusedObsSet,
         props.focusedTarget,
         state.zoom(TwoPanelState.selected).value.reuseByValue
        )
      ) { (_, _) => params =>
        val (focusedObsSet, focusedTarget, selected) = params
        (focusedObsSet, focusedTarget, selected.get) match {
          case (Some(_), _, _)                    => selected.set(SelectedPanel.editor)
          case (None, Some(_), _)                 => selected.set(SelectedPanel.editor)
          case (None, None, SelectedPanel.Editor) => selected.set(SelectedPanel.Summary)
          case _                                  => Callback.empty
        }
      }
      // Measure its size
      .useResizeDetector()
      // Initial target layout
      .useStateViewWithReuse(defaultTargetLayouts)
      // Keep a record of the initial target layouut
      .useMemo(())(_ => defaultTargetLayouts)
      // Load the config from user prefrences
      .useEffectWithDepsBy((p, _, _, _, _) => p.userId) {
        (props, panels, _, layout, defaultLayout) => _ =>
          {
            implicit val ctx = props.ctx

            TabGridPreferencesQuery
              .queryWithDefault[IO](props.userId,
                                    GridLayoutSection.TargetLayout,
                                    ResizableSection.TargetsTree,
                                    (Constants.InitialTreeWidth.toInt, defaultLayout)
              )
              .attempt
              .flatMap {
                case Right((w, l)) =>
                  (panels
                    .mod(
                      TwoPanelState.treeWidth.replace(w.toDouble)
                    ) *> layout.mod(o => mergeMap(o, l)))
                    .to[IO]
                case Left(_)       =>
                  IO.unit
              }
          }
      }
      .useSingleEffect(debounce = 1.second)
      .renderWithReuse { (props, twoPanelState, resize, layout, defaultLayout, debouncer) =>
        implicit val ctx = props.ctx
        <.div(
          AsterismGroupLiveQuery(
            props.programId,
            Reuse(renderFn _)(props, twoPanelState, defaultLayout, layout, resize, debouncer)
          )
        ).withRef(resize.ref)
      }

}
