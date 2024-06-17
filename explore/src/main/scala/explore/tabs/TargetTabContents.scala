// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import explore.*
import explore.components.FocusedStatus
import explore.components.Tile
import explore.components.TileController
import explore.components.ui.ExploreStyles
import explore.data.KeyedIndexedList
import explore.model.*
import explore.model.AppContext
import explore.model.ObsSummary
import explore.model.enums.AppTab
import explore.model.enums.GridLayoutSection
import explore.model.enums.SelectedPanel
import explore.model.syntax.all.*
import explore.observationtree.AsterismGroupObsList
import explore.shortcuts.*
import explore.shortcuts.given
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
import lucuma.react.common.*
import lucuma.react.hotkeys.*
import lucuma.react.hotkeys.hooks.*
import lucuma.react.resizeDetector.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.refined.*
import lucuma.schemas.model.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import monocle.Iso
import monocle.Traversal
import queries.schemas.odb.ObsQueries

import java.time.Instant
import scala.collection.immutable.SortedSet
import scala.scalajs.LinkingInfo

case class TargetTabContents(
  userId:           Option[User.Id],
  programId:        Program.Id,
  programSummaries: UndoContext[ProgramSummaries],
  userPreferences:  View[UserPreferences],
  focused:          Focused,
  searching:        View[Set[Target.Id]],
  expandedIds:      View[SortedSet[ObsIdSet]],
  readonly:         Boolean
) extends ReactFnProps(TargetTabContents.component):
  val targets: UndoSetter[TargetList] = props.programSummaries.zoom(ProgramSummaries.targets)

  val globalPreferences: View[GlobalPreferences] =
    userPreferences.zoom(UserPreferences.globalPreferences)

object TargetTabContents extends TwoPanels:
  private type Props = TargetTabContents

  private def renderFn(
    props:             Props,
    ctx:               AppContext[IO],
    selectedView:      View[SelectedPanel],
    selectedTargetIds: View[List[Target.Id]],
    fullScreen:        View[AladinFullScreen],
    resize:            UseResizeDetectorReturn
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
        selectTargetOrSummary,
        selectedTargetIds,
        props.readonly
      )

    def findAsterismGroup(obsIds: ObsIdSet, agl: AsterismGroupList): Option[AsterismGroup] =
      agl.find((agObsIds, _) => obsIds.subsetOf(agObsIds)).map(AsterismGroup.fromTuple)

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
    def renderSummary: Tile =
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
          selectObservationAndTarget(props.expandedIds),
          selectTargetOrSummary,
          renderInTitle,
          selectedTargetIds,
          props.programSummaries,
          props.readonly
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
      val groupIds = asterismGroup.obsIds

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
                  const,
                  _,
                  _,
                  _,
                  Some(conf),
                  _,
                  posAngle,
                  Some(wavelength),
                  _,
                  _,
                  _
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
          setCurrentTarget(props.programId, idsToEdit),
          otherObsCount(idsToEdit),
          props.searching,
          title,
          props.globalPreferences,
          props.readonly,
          backButton = backButton.some
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
          vizTimeView.get,
          none,
          Nil,
          props.globalPreferences.get
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
        props.globalPreferences,
        props.readonly
      )

      val skyPlotTile =
        ElevationPlotTile.elevationPlotTile(
          props.userId,
          targetId.some,
          none,
          // TODO PM correct the coordinates
          CoordinatesAtVizTime(Target.Sidereal.baseCoordinates.get(target)).some,
          none,
          none,
          Nil,
          props.globalPreferences.get
        )

      List(renderSummary, targetTile, skyPlotTile)
    }

    val optSelected: Option[Either[Target.Id, ObsIdSet]] = props.focused match
      case Focused(Some(obsIdSet), _, _)    => obsIdSet.asRight.some
      case Focused(None, Some(targetId), _) => targetId.asLeft.some
      case _                                => none

    val renderNonSiderealTargetEditor: List[Tile] =
      List(
        renderSummary,
        Tile("nonSiderealTarget".refined, "Non-sidereal target")(_ =>
          <.div("Editing of Non-Sidereal targets not supported")
        )
      )

    val rightSide = { (resize: UseResizeDetectorReturn) =>
      val tileListKeyOpt = optSelected
        .flatMap(
          _ match
            case Left(targetId) =>
              props.targets.get
                .get(targetId)
                .map {
                  case Nonsidereal(_, _, _)     =>
                    (renderNonSiderealTargetEditor, "target-non-sidereal-controller")
                  case s @ Sidereal(_, _, _, _) =>
                    (renderSiderealTargetEditor(resize, targetId, s), "target-sidereal-controller")
                }
            case Right(obsIds)  =>
              findAsterismGroup(obsIds, props.programSummaries.get.asterismGroups)
                .map(asterismGroup =>
                  (renderAsterismEditor(resize, obsIds, asterismGroup), "target-obs-controller")
                )
        )

      val (tiles, key, current, default) =
        tileListKeyOpt.fold(
          (List(renderSummary),
           "target-summary-controller",
           ExploreGridLayouts.targets.defaultSingleLayouts,
           ExploreGridLayouts.targets.defaultSingleLayouts
          )
        )((tiles, key) =>
          (tiles,
           key,
           props.userPreferences.get.targetTabLayout,
           ExploreGridLayouts.targets.defaultTargetLayouts
          )
        )

      TileController(
        props.userId,
        resize.width.getOrElse(1),
        default,
        current,
        tiles,
        GridLayoutSection.TargetLayout,
        backButton.some,
        Option.when(tileListKeyOpt.isEmpty)(ExploreStyles.SingleTileMaximized),
        storeLayout = tileListKeyOpt.nonEmpty
      ).withKey(key): VdomNode
      // withKey is required for the controller to clear it's state between the different
      // layouts or it can get into an invalid state.
    }

    React.Fragment(
      if (LinkingInfo.developmentMode) FocusedStatus(AppTab.Targets, props.programId, props.focused)
      else EmptyVdom,
      makeOneOrTwoPanels(
        selectedView,
        targetTree(props.programSummaries),
        rightSide,
        RightSideCardinality.Multi,
        resize
      )
    )
  }

  private def applyObs(
    obsIds:           List[(Observation.Id, List[Target.Id])],
    programSummaries: UndoSetter[ProgramSummaries],
    ctx:              AppContext[IO],
    expandedIds:      View[SortedSet[ObsIdSet]]
  ): IO[Unit] =
    import ctx.given
    obsIds
      .traverse { case (obsId, targetIds) =>
        ObsQueries
          .applyObservation[IO](obsId, targetIds)
          .map(o => programSummaries.get.cloneObsWithTargets(obsId, o.id, targetIds))
          .map(_.map(summ => (summ, targetIds)))
      }
      .flatMap(olist =>
        olist.sequence
          .foldMap(summList =>
            val newIds    = summList.map((summ, tid) => (summ.id, tid))
            val summaries = summList.map(_._1)
            ObservationPasteAction
              .paste(newIds, expandedIds)
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
      .useEffectWithDepsBy((props, _, _) => props.focused) { (_, _, selected) => focused =>
        (focused, selected.get) match
          case (Focused(Some(_), _, _), _)                    => selected.set(SelectedPanel.Editor)
          case (Focused(None, Some(_), _), _)                 => selected.set(SelectedPanel.Editor)
          case (Focused(None, None, _), SelectedPanel.Editor) =>
            selected.set(SelectedPanel.Summary)
          case _                                              => Callback.empty
      }
      .useStateViewBy((props, _, _) => props.focused.target.toList)
      .useEffectWithDepsBy((props, _, _, _) => props.focused.target)((_, _, _, selIds) =>
        _.foldMap(focusedTarget => selIds.set(List(focusedTarget)))
      )
      .useGlobalHotkeysWithDepsBy((props, ctx, _, selIds) =>
        (props.focused, props.programSummaries.get.asterismGroups, props.readonly, selIds.get)
      ) { (props, ctx, _, _) => (target, asterismGroups, readonly, selectedIds) =>
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
            ExploreClipboard.get
              .flatMap {
                case LocalClipboard.CopiedObservations(id) =>
                  val obsAndTargets =
                    props.focused.obsSet
                      // This with some targets on the tree seelected
                      .map(i => id.idSet.toList.map((_, asterismGroups.get(i).foldMap(_.toList))))
                      // These are targets on the table
                      .getOrElse {
                        for {
                          tid <- selectedIds
                          oid <- id.idSet.toList
                        } yield (oid, List(tid))
                      }

                  if (obsAndTargets.nonEmpty)
                    // Apply the obs to selected targets on the tree
                    applyObs(
                      obsAndTargets,
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
              }
              .runAsync
              .unless_(readonly)

          case GoToSummary =>
            ctx.pushPage(AppTab.Targets, props.programId, Focused.None)
        }
        UseHotkeysProps((GoToSummary :: (CopyKeys ::: PasteKeys)).toHotKeys, callbacks)
      }
      // full screen aladin
      .useStateView(AladinFullScreen.Normal)
      // Measure its size
      .useResizeDetector()
      .render(renderFn)
