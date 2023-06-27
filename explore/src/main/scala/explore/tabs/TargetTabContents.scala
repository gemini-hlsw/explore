// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import _root_.react.common.*
import _root_.react.draggable.Axis
import _root_.react.gridlayout.*
import _root_.react.hotkeys.*
import _root_.react.hotkeys.hooks.*
import _root_.react.resizeDetector.*
import _root_.react.resizeDetector.hooks.*
import cats.Order.*
import cats.data.NonEmptyList
import cats.effect.IO
import cats.syntax.all.*
import cats.syntax.all.given
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import crystal.react.reuse.*
import eu.timepit.refined.auto.*
import eu.timepit.refined.types.numeric.NonNegInt
import explore.Icons
import explore.*
import explore.cache.ProgramCache
import explore.common.AsterismQueries.*
import explore.common.UserPreferencesQueries.*
import explore.components.Tile
import explore.components.TileController
import explore.components.ui.ExploreStyles
import explore.data.KeyedIndexedList
import explore.given
import explore.model.ObsSummary
import explore.model.*
import explore.model.enums.AppTab
import explore.model.enums.GridLayoutSection
import explore.model.enums.SelectedPanel
import explore.model.layout.*
import explore.model.layout.unsafe.given
import explore.model.reusability.given
import explore.model.syntax.all.*
import explore.observationtree.AsterismGroupObsList
import explore.optics.*
import explore.optics.all.*
import explore.shortcuts.*
import explore.shortcuts.given
import explore.syntax.ui.*
import explore.syntax.ui.given
import explore.targets.ObservationPasteAction
import explore.targets.TargetPasteAction
import explore.targets.TargetSummaryTable
import explore.undo.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Coordinates
import lucuma.core.model.CoordinatesAtVizTime
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.Target.Nonsidereal
import lucuma.core.model.Target.Sidereal
import lucuma.core.model.User
import lucuma.refined.*
import lucuma.schemas.model.*
import lucuma.ui.DefaultPendingRender
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import lucuma.ui.utils.*
import monocle.Iso
import monocle.Traversal
import org.scalajs.dom.window
import queries.common.AsterismQueriesGQL.*
import queries.common.ObsQueriesGQL
import queries.common.TargetQueriesGQL
import queries.common.UserPreferencesQueriesGQL.*
import queries.schemas.odb.ObsQueries

import java.time.Instant
import scala.collection.immutable.SortedMap
import scala.collection.immutable.SortedSet
import scala.concurrent.duration.*

case class TargetTabContents(
  userId:           Option[User.Id],
  programId:        Program.Id,
  programSummaries: UndoContext[ProgramSummaries],
  focused:          Focused,
  searching:        View[Set[Target.Id]],
  expandedIds:      View[SortedSet[ObsIdSet]]
) extends ReactFnProps(TargetTabContents.component):
  val targets: UndoSetter[TargetList] = props.programSummaries.zoom(ProgramSummaries.targets)

object TargetTabContents extends TwoPanels:
  private type Props = TargetTabContents

  private val SummaryHeight: NonNegInt     = 6.refined
  private val SummaryMinHeight: NonNegInt  = 4.refined
  private val TargetHeight: NonNegInt      = 18.refined
  private val TargetMinHeight: NonNegInt   = 15.refined
  private val SkyPlotHeight: NonNegInt     = 9.refined
  private val SkyPlotMinHeight: NonNegInt  = 6.refined
  private val TileMinWidth: NonNegInt      = 5.refined
  private val DefaultWidth: NonNegInt      = 10.refined
  private val DefaultLargeWidth: NonNegInt = 12.refined

  private val layoutMedium: Layout = Layout(
    List(
      LayoutItem(
        i = ObsTabTilesIds.TargetSummaryId.id.value,
        x = 0,
        y = 0,
        w = DefaultWidth.value,
        h = SummaryHeight.value,
        minH = SummaryMinHeight.value,
        minW = TileMinWidth.value
      ),
      LayoutItem(
        i = ObsTabTilesIds.TargetId.id.value,
        x = 0,
        y = SummaryHeight.value,
        w = DefaultWidth.value,
        h = TargetHeight.value,
        minH = TargetMinHeight.value,
        minW = TileMinWidth.value
      ),
      LayoutItem(
        i = ObsTabTilesIds.PlotId.id.value,
        x = 0,
        y = SummaryHeight.value + TargetHeight.value,
        w = DefaultWidth.value,
        h = SkyPlotHeight.value,
        minH = SkyPlotMinHeight.value,
        minW = TileMinWidth.value
      )
    )
  )

  private val defaultLayouts = defineStdLayouts(
    Map(
      (BreakpointName.lg,
       layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(layoutMedium)
      ),
      (BreakpointName.md, layoutMedium)
    )
  )

  private val singleLayoutMedium: Layout = Layout(
    List(
      LayoutItem(
        i = ObsTabTilesIds.TargetSummaryId.id.value,
        x = 0,
        y = 0,
        w = DefaultWidth.value,
        h = 0, // This doesn't matter, we are forcing 100%.
        minH = SummaryMinHeight.value,
        minW = TileMinWidth.value,
        static = true
      )
    )
  )

  private val defaultSingleLayouts = defineStdLayouts(
    Map(
      (BreakpointName.lg,
       layoutItems.andThen(layoutItemWidth).replace(DefaultLargeWidth)(singleLayoutMedium)
      ),
      (BreakpointName.md, singleLayoutMedium)
    )
  )

  private def renderFn(
    props:             Props,
    selectedView:      View[SelectedPanel],
    layouts:           View[Pot[LayoutsMap]],
    resize:            UseResizeDetectorReturn,
    fullScreen:        View[AladinFullScreen],
    selectedTargetIds: View[List[Target.Id]],
    ctx:               AppContext[IO]
  ): VdomNode = {
    import ctx.given

    def otherObsCount(obsIds: ObsIdSet)(targetId: Target.Id): Int =
      props.targets.get
        .get(targetId)
        .fold(0)(tg =>
          (props.programSummaries.get.targetObservations
            .get(targetId)
            .orEmpty -- obsIds.toSortedSet).size
        )

    def targetTree(programSummaries: UndoContext[ProgramSummaries]) =
      AsterismGroupObsList(
        props.programId,
        props.focused,
        props.expandedIds,
        programSummaries,
        selectTargetOrSummary _,
        selectedTargetIds
      )

    def findAsterismGroup(obsIds: ObsIdSet, agl: AsterismGroupList): Option[AsterismGroup] =
      agl.find((agObsIds, targetIds) => obsIds.subsetOf(agObsIds)).map(AsterismGroup.fromTuple)

    def setPage(focused: Focused): Callback =
      ctx.pushPage(AppTab.Targets, props.programId, focused)

    def selectObservationAndTarget(expandedIds: View[SortedSet[ObsIdSet]])(
      obsId:    Observation.Id,
      targetId: Target.Id
    ): Callback = {
      val obsIdSet = ObsIdSet.one(obsId)
      findAsterismGroup(obsIdSet, props.programSummaries.get.asterismGroups)
        .map(ag => expandedIds.mod(_ + ag.obsIds))
        .orEmpty >>
        setPage(Focused(obsIdSet.some, targetId.some))
    }

    def selectTargetOrSummary(oTargetId: Option[Target.Id]): Callback =
      oTargetId.fold(
        selectedView.set(SelectedPanel.Summary) *>
          setPage(Focused.None)
      )(targetId => setPage(Focused.target(targetId)))

    def onModAsterismsWithObs(
      groupIds:  ObsIdSet,
      editedIds: ObsIdSet
    )(ps: ProgramSummaries): Callback =
      findAsterismGroup(editedIds, ps.asterismGroups).foldMap { tlg =>
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

            withOldAndNew.filter(ids => ps.asterismGroups.contains(ids)) // clean up
          }
      }

    val backButton: VdomNode =
      makeBackButton(props.programId, AppTab.Targets, selectedView, ctx)

    /**
     * Render the summary table.
     */
    def renderSummary(single: Boolean): Tile =
      Tile(
        ObsTabTilesIds.TargetSummaryId.id,
        "Target Summary",
        backButton.some
      )(renderInTitle =>
        TargetSummaryTable(
          props.userId,
          props.programId,
          props.targets.model,
          props.programSummaries.get.targetObservations,
          selectObservationAndTarget(props.expandedIds) _,
          selectTargetOrSummary _,
          renderInTitle,
          selectedTargetIds,
          props.programSummaries
        )
      )

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
    def renderAsterismEditor(
      resize:        UseResizeDetectorReturn,
      idsToEdit:     ObsIdSet,
      asterismGroup: AsterismGroup
    ): List[Tile] = {
      val groupIds  = asterismGroup.obsIds
      val targetIds = asterismGroup.targetIds

      val getVizTime: ProgramSummaries => Option[Instant] = a =>
        for
          id <- idsToEdit.single
          o  <- a.observations.getValue(id)
          t  <- o.visualizationTime
        yield t

      def modVizTime(
        mod: Option[Instant] => Option[Instant]
      ): ProgramSummaries => ProgramSummaries = ps =>
        idsToEdit.single
          .map(i =>
            ProgramSummaries.observations
              .filterIndex((id: Observation.Id) => id === i)
              .andThen(KeyedIndexedList.value)
              .andThen(ObsSummary.visualizationTime)
              .modify(mod)(ps)
          )
          .getOrElse(ps)

      val traversal: Traversal[ObservationList, AsterismIds] =
        Iso
          .id[ObservationList]
          .filterIndex((id: Observation.Id) => idsToEdit.contains(id))
          .andThen(KeyedIndexedList.value)
          .andThen(ObsSummary.scienceTargetIds)

      val asterismView: View[AsterismIds] =
        CloneListView(
          props.programSummaries.model
            .withOnMod(onModAsterismsWithObs(groupIds, idsToEdit))
            .zoom(ProgramSummaries.observations.andThen(traversal))
        )

      val vizTimeView: View[Option[Instant]] =
        props.programSummaries.model.zoom(getVizTime)(modVizTime)

      val title = idsToEdit.single match {
        case Some(id) => s"Observation $id"
        case None     => s"Editing ${idsToEdit.size} Asterisms"
      }

      val obsConf = idsToEdit.single match {
        case Some(id) =>
          props.programSummaries.get.observations.values.collect {
            case ObsSummary(
                  obsId,
                  _,
                  _,
                  _,
                  _,
                  _,
                  _,
                  const,
                  _,
                  _,
                  _,
                  Some(conf),
                  _,
                  posAngle,
                  Some(wavelength)
                ) if obsId === id =>
              (const, conf.toBasicConfiguration, posAngle, wavelength)
          }.headOption
        case _        => None
      }

      val constraints                               = obsConf.map(_._1)
      val configuration: Option[BasicConfiguration] = obsConf.map(_._2)
      val wavelength                                = obsConf.map(_._4)

      def setCurrentTarget(programId: Program.Id, oids: ObsIdSet)(
        tid: Option[Target.Id],
        via: SetRouteVia
      ): Callback =
        ctx.setPageVia(AppTab.Targets, programId, Focused(oids.some, tid), via)

      val asterismEditorTile =
        AsterismEditorTile.asterismEditorTile(
          props.userId,
          props.programId,
          idsToEdit,
          asterismView,
          props.targets,
          configuration,
          vizTimeView,
          ObsConfiguration(configuration, none, constraints, wavelength, none, none, none),
          props.focused.target,
          setCurrentTarget(props.programId, idsToEdit) _,
          otherObsCount(idsToEdit) _,
          props.searching,
          title,
          backButton.some
        )

      val selectedCoordinates: Option[Coordinates] =
        props.focused.target.flatMap(id =>
          props.targets.get
            .get(id)
            .flatMap {
              case t @ Target.Sidereal(_, _, _, _) =>
                // TODO PM correction
                Target.Sidereal.baseCoordinates.get(t).some
              case _                               => none
            }
        )

      val skyPlotTile =
        ElevationPlotTile.elevationPlotTile(
          props.userId,
          props.focused.target,
          configuration.map(_.siteFor),
          selectedCoordinates.map(CoordinatesAtVizTime(_)),
          vizTimeView.get
        )

      List(asterismEditorTile, skyPlotTile)
    }

    def renderSiderealTargetEditor(
      resize:   UseResizeDetectorReturn,
      targetId: Target.Id,
      target:   Target.Sidereal
    ): List[Tile] = {
      val getTarget: TargetList => Target.Sidereal = _ => target

      def modTarget(mod: Target.Sidereal => Target.Sidereal): TargetList => TargetList =
        _.updatedWith(targetId) {
          case Some(s @ Target.Sidereal(_, _, _, _)) => mod(s).some
          case other                                 => other
        }

      val title = s"Editing Target ${target.name.value} [$targetId]"

      val targetTile = SiderealTargetEditorTile.noObsSiderealTargetEditorTile(
        props.userId,
        targetId,
        props.targets.zoom(getTarget, modTarget),
        props.searching,
        title,
        fullScreen,
        backButton.some
      )

      val skyPlotTile =
        ElevationPlotTile.elevationPlotTile(
          props.userId,
          targetId.some,
          none,
          // TODO PM correct the coordinates
          CoordinatesAtVizTime(Target.Sidereal.baseCoordinates.get(target)).some,
          none
        )

      List(renderSummary(false), targetTile, skyPlotTile)
    }

    val optSelected: Option[Either[Target.Id, ObsIdSet]] = props.focused match
      case Focused(Some(obsIdSet), _)    => obsIdSet.asRight.some
      case Focused(None, Some(targetId)) => targetId.asLeft.some
      case _                             => none

    val renderNonSiderealTargetEditor: List[Tile] =
      List(
        renderSummary(false),
        Tile("nonSiderealTarget".refined, "Non-sidereal target")(_ =>
          <.div("Editing of Non-Sidereal targets not supported")
        )
      )

    val rightSide = { (resize: UseResizeDetectorReturn) =>
      val tileListObSelectedOpt: Option[(List[Tile], Boolean)] = optSelected.flatMap(
        _ match
          case Left(targetId) =>
            props.targets.get
              .get(targetId)
              .map {
                case Nonsidereal(_, _, _)     => (renderNonSiderealTargetEditor, false)
                case s @ Sidereal(_, _, _, _) =>
                  (renderSiderealTargetEditor(resize, targetId, s), false)
              }
          case Right(obsIds)  =>
            findAsterismGroup(obsIds, props.programSummaries.get.asterismGroups)
              .map(asterismGroup => (renderAsterismEditor(resize, obsIds, asterismGroup), true))
      )

      val tileList: Option[List[Tile]] = tileListObSelectedOpt.map(_._1)
      val obsSelected: Boolean         = tileListObSelectedOpt.map(_._2).exists(identity)

      layouts.renderPotView(l =>
        TileController(
          props.userId,
          resize.width.getOrElse(1),
          tileList.fold(defaultSingleLayouts)(_ => defaultLayouts),
          tileList.fold(defaultSingleLayouts)(_ => l),
          tileList.getOrElse(List(renderSummary(true))),
          GridLayoutSection.TargetLayout,
          Option.when(tileList.isEmpty)(ExploreStyles.SingleTileMaximized),
          storeLayout = tileList.nonEmpty
        ).withKey(if (obsSelected) "target-obs-controller" else "target-summary-controller")
        // We need different tile controller keys when in observations than when in target summary,
        // so that it clears its internal state.
      )
    }

    makeOneOrTwoPanels(
      selectedView,
      targetTree(props.programSummaries),
      rightSide,
      RightSideCardinality.Multi,
      resize
    )
  }

  private def applyObs(
    programId:        Program.Id,
    obsIds:           List[Observation.Id],
    targetIds:        List[Target.Id],
    programSummaries: UndoSetter[ProgramSummaries],
    ctx:              AppContext[IO],
    expandedIds:      View[SortedSet[ObsIdSet]]
  ): IO[Unit] =
    import ctx.given
    (obsIds, targetIds).tupled
      .traverse((obsId, tid) =>
        ObsQueries
          .applyObservation[IO](obsId, List(tid))
          .map(o => programSummaries.get.cloneObsWithTargets(obsId, o.id, List(tid)))
          .map(_.map(summ => (summ, tid)))
      )
      .flatMap(olist =>
        olist.sequence
          .foldMap(summList =>
            val newIds    = summList.map((summ, tid) => (summ.id, tid))
            val summaries = summList.map(_._1)
            ObservationPasteAction
              .paste(programId, newIds, expandedIds)
              .set(programSummaries)(summaries.some)
              .toAsync
          )
      )
      .void

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      // Two panel state
      .useStateView[SelectedPanel](SelectedPanel.Uninitialized)
      .useEffectWithDepsBy((props, _, state) => (props.focused, state.reuseByValue)) {
        (_, _, selected) => (focused, _) =>
          (focused, selected.get) match
            case (Focused(Some(_), _), _)                    => selected.set(SelectedPanel.Editor)
            case (Focused(None, Some(_)), _)                 => selected.set(SelectedPanel.Editor)
            case (Focused(None, None), SelectedPanel.Editor) => selected.set(SelectedPanel.Summary)
            case _                                           => Callback.empty
      }
      // Measure its size
      .useResizeDetector()
      // Initial target layout
      .useStateView(Pot.pending[LayoutsMap])
      // Load the config from user prefrences
      .useEffectWithDepsBy((p, _, _, _, _) => p.userId) { (props, ctx, _, _, layout) => _ =>
        import ctx.given

        GridLayouts
          .queryWithDefault[IO](
            props.userId,
            GridLayoutSection.TargetLayout,
            defaultLayouts
          )
          .attempt
          .flatMap {
            case Right(dbLayout) =>
              layout
                .mod(
                  _.fold(
                    mergeMap(dbLayout, defaultLayouts).ready,
                    _ => mergeMap(dbLayout, defaultLayouts).ready,
                    cur => mergeMap(dbLayout, cur).ready
                  )
                )
                .toAsync
            case Left(_)         => IO.unit
          }
      }
      // Selected targets on the summary table
      .useStateViewBy((props, _, _, _, _) => props.focused.target.toList)
      .useEffectWithDepsBy((props, _, _, _, _, _) => props.focused.target)(
        (_, _, _, _, _, selIds) => _.foldMap(focusedTarget => selIds.set(List(focusedTarget)))
      )
      .useGlobalHotkeysWithDepsBy((props, ctx, _, _, _, selIds) =>
        (props.focused, props.programSummaries.get.asterismGroups, selIds.get)
      ) { (props, ctx, _, _, _, _) => (target, asterismGroups, selectedIds) =>
        import ctx.given

        def selectObsIds: ObsIdSet => IO[Unit] =
          obsIds => ctx.pushPage(AppTab.Targets, props.programId, Focused.obsSet(obsIds)).toAsync

        def callbacks: ShortcutCallbacks = {
          case CopyAlt1 | CopyAlt2 =>
            target.obsSet
              .map(ids =>
                ExploreClipboard
                  .set(LocalClipboard.CopiedObservations(ids))
                  .withToast(s"Copied obs ${ids.idSet.toList.mkString(", ")}")
              )
              .orElse(
                TargetIdSet
                  .fromTargetIdList(selectedIds)
                  .map(tids =>
                    ExploreClipboard
                      .set(LocalClipboard.CopiedTargets(tids))
                      .withToast(s"Copied targets ${tids.toList.mkString(", ")}")
                  )
              )
              .orUnit
              .runAsync

          case PasteAlt1 | PasteAlt2 =>
            ExploreClipboard.get.flatMap {
              case LocalClipboard.CopiedObservations(id) =>
                val treeTargets =
                  props.focused.obsSet
                    .flatMap(i => asterismGroups.get(i).map(_.toList))
                    .getOrElse(selectedIds)

                if (treeTargets.nonEmpty)
                  // Apply the obs to selected targets on the tree
                  applyObs(
                    props.programId,
                    id.idSet.toList,
                    treeTargets,
                    props.programSummaries,
                    ctx,
                    props.expandedIds
                  ).withToast(s"Pasting obs ${id.idSet.toList.mkString(", ")}")
                else IO.unit

              case LocalClipboard.CopiedTargets(tids) =>
                props.focused.obsSet
                  .foldMap(obsIds =>
                    // Only want to paste targets that aren't already in the target asterism or
                    // undo is messed up.
                    // If all the targets are already there, do nothing.
                    val targetAsterism =
                      props.programSummaries.get.asterismGroups.findContainingObsIds(obsIds)
                    targetAsterism
                      .flatMap(ag => tids.removeSet(ag.targetIds))
                      .foldMap(uniqueTids =>
                        TargetPasteAction
                          .pasteTargets(
                            props.programId,
                            obsIds,
                            uniqueTids,
                            selectObsIds,
                            props.expandedIds
                          )
                          .set(props.programSummaries)(())
                          .toAsync
                      )
                  )

              case _ => IO.unit
            }.runAsync

          case GoToSummary =>
            ctx.pushPage(AppTab.Targets, props.programId, Focused.None)
        }
        UseHotkeysProps((GoToSummary :: (CopyKeys ::: PasteKeys)).toHotKeys, callbacks)
      }
      // full screen aladin
      .useStateView(AladinFullScreen.Normal)
      .render {
        (
          props,
          ctx,
          twoPanelState,
          resize,
          layout,
          selectedTargetIds,
          fullScreen
        ) =>
          renderFn(
            props,
            twoPanelState,
            layout,
            resize,
            fullScreen,
            selectedTargetIds,
            ctx
          )
      }
