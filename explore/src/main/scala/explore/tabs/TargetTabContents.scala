// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.Order.given
import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.*
import explore.actions.ObservationPasteIntoAsterismAction
import explore.components.FocusedStatus
import explore.components.Tile
import explore.components.TileController
import explore.components.ui.ExploreStyles
import explore.data.KeyedIndexedList
import explore.model.*
import explore.model.AppContext
import explore.model.GuideStarSelection
import explore.model.GuideStarSelection.AgsSelection
import explore.model.Observation
import explore.model.OnCloneParameters
import explore.model.TargetEditObsInfo
import explore.model.enums.AppTab
import explore.model.enums.GridLayoutSection
import explore.model.enums.SelectedPanel
import explore.model.reusability.given
import explore.model.syntax.all.*
import explore.observationtree.AsterismGroupObsList
import explore.shortcuts.*
import explore.shortcuts.given
import explore.targeteditor.plots.ObjectPlotData
import explore.targeteditor.plots.PlotData
import explore.targets.TargetPasteAction
import explore.targets.TargetSummaryBody
import explore.targets.TargetSummaryTileState
import explore.targets.TargetSummaryTitle
import explore.undo.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.enums.Site
import lucuma.core.model.ObjectTracking
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.TimeSpan
import lucuma.react.common.*
import lucuma.react.hotkeys.*
import lucuma.react.hotkeys.hooks.*
import lucuma.react.resizeDetector.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.*
import lucuma.ui.optics.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import monocle.Iso
import org.typelevel.log4cats.Logger
import queries.schemas.odb.ObsQueries
import explore.targeteditor.AsterismEditorTile

import java.time.Instant
import scala.collection.immutable.SortedSet
import scala.scalajs.LinkingInfo

case class TargetTabContents(
  programId:        Program.Id,
  userId:           Option[User.Id],
  programSummaries: UndoContext[ProgramSummaries],
  userPreferences:  View[UserPreferences],
  focused:          Focused,
  searching:        View[Set[Target.Id]],
  expandedIds:      View[SortedSet[ObsIdSet]],
  readonly:         Boolean
) extends ReactFnProps(TargetTabContents.component):
  private val targets: UndoSetter[TargetList] = programSummaries.zoom(ProgramSummaries.targets)

  private val focusedIds: Option[Either[Target.Id, ObsIdSet]] =
    focused match
      case Focused(Some(obsIdSet), _, _)    => obsIdSet.asRight.some
      case Focused(None, Some(targetId), _) => targetId.asLeft.some
      case _                                => none

  val focusedSummaryTargetId: Option[Target.Id] =
    focusedIds.flatMap(_.left.toOption)

  val focusedAsterismTargetId: Option[Target.Id] =
    focused match
      case Focused(Some(_), Some(targetId), _) => targetId.some
      case _                                   => none

  private def sitesForTarget(targetId: Target.Id): List[Site] =
    programSummaries.get.targetObservations
      .get(targetId)
      .foldMap: obsIds =>
        obsIds.toList
          .map: obsId =>
            programSummaries.get.observations
              .getValue(obsId)
              .flatMap(_.observingMode.map(_.siteFor))
          .flattenOption

  private val obsAndTargets: UndoSetter[ObservationsAndTargets] =
    programSummaries.zoom((ProgramSummaries.observations, ProgramSummaries.targets).disjointZip)

  private val observations: ObservationList = obsAndTargets.get._1

  private val globalPreferences: View[GlobalPreferences] =
    userPreferences.zoom(UserPreferences.globalPreferences)

object TargetTabContents extends TwoPanels:
  private type Props = TargetTabContents

  private def applyObs(
    obsIds:           List[(Observation.Id, List[Target.Id])],
    programSummaries: UndoSetter[ProgramSummaries],
    expandedIds:      View[SortedSet[ObsIdSet]]
  )(using FetchClient[IO, ObservationDB], Logger[IO]): IO[Unit] =
    obsIds
      .traverse: (obsId, targetIds) =>
        ObsQueries
          .applyObservation[IO](obsId, onTargets = targetIds.some)
          .map(o => programSummaries.get.getObsClone(obsId, o.id, withTargets = targetIds.some))
          .map(_.map(obs => (obs, targetIds)))
      .flatMap: olist =>
        olist.sequence
          .foldMap: obsWithTargetList =>
            val newIds: List[(Observation.Id, List[Target.Id])] =
              obsWithTargetList.map((obs, tids) => (obs.id, tids))

            val observations: List[Observation] =
              obsWithTargetList.map(_._1)

            ObservationPasteIntoAsterismAction(newIds, expandedIds.async.mod)
              .set(programSummaries)(observations.some)
              .toAsync
      .void

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView[SelectedPanel](SelectedPanel.Uninitialized) // Two panel state
      .useEffectWithDepsBy((props, _, _) => props.focused): (_, _, selectedPanel) =>
        focused =>
          (focused, selectedPanel.get) match
            case (Focused(Some(_), _, _), _)                    => selectedPanel.set(SelectedPanel.Editor)
            case (Focused(None, Some(_), _), _)                 => selectedPanel.set(SelectedPanel.Editor)
            case (Focused(None, None, _), SelectedPanel.Editor) =>
              selectedPanel.set(SelectedPanel.Summary)
            case _                                              => Callback.empty
      .useStateViewBy((props, _, _) => List.empty[Target.Id])   // Selected targets on table
      .useLayoutEffectWithDepsBy((props, _, _, _) => props.focused.target):
        // If a target enters edit mode, unselect the rows.
        (_, _, _, selTargetIds) => _.foldMap(_ => selTargetIds.set(List.empty))
      .useMemoBy((props, _, _, selTargetIds) => (props.focusedIds, selTargetIds.get)): // Selected observations (right) or targets (left)
        (_, _, _, _) =>
          (focusedIds, selTargetIds) =>
            focusedIds
              .map(_.leftMap(TargetIdSet.one(_)))
              .orElse:
                TargetIdSet.fromTargetIdList(selTargetIds).map(_.asLeft)
      .useState[LocalClipboard](
        LocalClipboard.Empty
      )                                      // shadowClipboard (a copy of the clipboard as state)
      .useEffectOnMountBy: (_, ctx, _, _, _, shadowClipboard) => // initialize shadowClipboard
        import ctx.given
        ExploreClipboard.get.flatMap(shadowClipboard.setStateAsync)
      .useCallbackWithDepsBy((_, _, _, _, selIdsOpt, _) => selIdsOpt): // COPY Action Callback
        (_, ctx, _, _, _, shadowClipboard) =>
          selIdsOpt =>
            import ctx.given

            selIdsOpt.value
              .map:
                _.fold[(LocalClipboard, String)](
                  tids =>
                    (LocalClipboard.CopiedTargets(tids),
                     s"Copied target(s) ${tids.toList.mkString(", ")}"
                    ),
                  oids =>
                    (LocalClipboard.CopiedObservations(oids),
                     s"Copied observation(s) ${oids.idSet.toList.mkString(", ")}"
                    )
                )
              .map: (newClipboard, toastText) =>
                (ExploreClipboard.set(newClipboard) >>
                  shadowClipboard.setStateAsync(newClipboard))
                  .withToast(toastText)
              .orEmpty
              .runAsync
      .useCallbackWithDepsBy((props, _, _, _, selIdsOpt, _, _) => // PASTE Action Callback
        (selIdsOpt.flatMap(_.left.toOption).map(_.toList).orEmpty, props.readonly)
      ): (props, ctx, _, _, _, _, _) =>
        (selTargetIds, readonly) =>
          import ctx.given

          val selectObsIds: ObsIdSet => IO[Unit] =
            obsIds => ctx.pushPage(AppTab.Targets, props.programId, Focused.obsSet(obsIds)).toAsync

          ExploreClipboard.get
            .flatMap {
              case LocalClipboard.CopiedObservations(copiedObsIdSet) =>
                val obsAndTargets: List[(Observation.Id, List[Target.Id])] =
                  props.focused.obsSet
                    .map: focusedObsIdSet => // This with some targets on the tree selected
                      copiedObsIdSet.idSet.toList.map: obsId =>
                        (obsId,
                         props.observations // All focused obs have the same asterism, so we can use head
                           .getValue(focusedObsIdSet.idSet.head)
                           .foldMap(_.scienceTargetIds)
                           .toList
                        )
                    .getOrElse: // These are targets on the table
                      for
                        tid <- selTargetIds
                        oid <- copiedObsIdSet.idSet.toList
                      yield (oid, List(tid))

                IO.whenA(obsAndTargets.nonEmpty): // Apply the obs to selected targets on the tree
                  applyObs(
                    obsAndTargets,
                    props.programSummaries,
                    props.expandedIds
                  ).withToast(s"Pasting obs ${copiedObsIdSet.idSet.toList.mkString(", ")}")

              case LocalClipboard.CopiedTargets(tids) =>
                props.focused.obsSet
                  .foldMap: obsIds =>
                    // Only want to paste targets that aren't already in the target asterism or
                    // undo is messed up.
                    // If all the targets are already there, do nothing.
                    val targetAsterism =
                      props.programSummaries.get.asterismGroups.findContainingObsIds(obsIds)
                    targetAsterism
                      .flatMap(ag => tids.removeSet(ag.targetIds))
                      .foldMap: uniqueTids =>
                        TargetPasteAction
                          .pasteTargets(
                            obsIds,
                            uniqueTids,
                            selectObsIds,
                            props.expandedIds
                          )
                          .set(props.programSummaries)(())
                          .toAsync

              case _ => IO.unit
            }
            .runAsync
            .unless_(readonly)
      .useGlobalHotkeysWithDepsBy((_, _, _, _, _, _, copyCallback, pasteCallback) =>
        (copyCallback, pasteCallback)
      ): (props, ctx, _, _, _, _, _, _) =>
        (copyCallback, pasteCallback) =>
          val callbacks: ShortcutCallbacks =
            case CopyAlt1 | CopyAlt2   => copyCallback
            case PasteAlt1 | PasteAlt2 => pasteCallback
            case GoToSummary           => ctx.pushPage(AppTab.Targets, props.programId, Focused.None)

          UseHotkeysProps((GoToSummary :: (CopyKeys ::: PasteKeys)).toHotKeys, callbacks)
      .useStateView(AladinFullScreen.Normal) // full screen aladin
      .useResizeDetector()                   // Measure its size
      .useStateView[GuideStarSelection](AgsSelection(none)) // required for aladin but not in use
      .render:
        (
          props,
          ctx,
          selectedPanelView,
          selectedTargetIds,
          selectedIdsOpt,
          shadowClipboard,
          copyCallback,
          pasteCallback,
          fullScreen,
          resize,
          guideStarSelection
        ) =>
          import ctx.given

          def getObsInfo(editing: Option[ObsIdSet])(targetId: Target.Id): TargetEditObsInfo =
            TargetEditObsInfo.fromProgramSummaries(targetId, editing, props.programSummaries.get)

          def findAsterismGroup(obsIds: ObsIdSet, agl: AsterismGroupList): Option[AsterismGroup] =
            agl.find((agObsIds, _) => obsIds.subsetOf(agObsIds)).map(AsterismGroup.fromTuple)

          def setPage(focused: Focused): Callback =
            ctx.pushPage(AppTab.Targets, props.programId, focused)

          def selectObservationAndTarget(expandedIds: View[SortedSet[ObsIdSet]])(
            obsId:    Observation.Id,
            targetId: Target.Id
          ): Callback =
            val obsIdSet = ObsIdSet.one(obsId)
            findAsterismGroup(obsIdSet, props.programSummaries.get.asterismGroups)
              .map(ag => expandedIds.mod(_ + ag.obsIds))
              .orEmpty >>
              setPage(Focused(obsIdSet.some, targetId.some))

          def focusTargetId(oTargetId: Option[Target.Id]): Callback =
            oTargetId.fold(
              selectedPanelView.set(SelectedPanel.Summary) *>
                setPage(Focused.None)
            ): targetId =>
              setPage(Focused.target(targetId))

          val targetTree: VdomNode =
            AsterismGroupObsList(
              props.programId,
              props.focused,
              props.expandedIds,
              props.programSummaries,
              selectedIdsOpt,
              shadowClipboard.value,
              focusTargetId,
              selectedTargetIds.set,
              props.programSummaries.undoableView(ProgramSummaries.targets).mod,
              copyCallback,
              pasteCallback,
              props.programSummaries.get.allocatedScienceBands,
              props.readonly
            )

          val backButton: VdomNode =
            makeBackButton(props.programId, AppTab.Targets, selectedPanelView, ctx)

          /**
           * Render the summary table.
           */
          val renderSummary: Tile[TargetSummaryTileState] = Tile(
            ObsTabTileIds.TargetSummaryId.id,
            s"Target Summary (${props.targets.get.size})",
            TargetSummaryTileState(Nil, none),
            backButton.some
          )(
            TargetSummaryBody(
              props.userId,
              props.programId,
              props.targets.model,
              props.programSummaries.get.targetObservations,
              props.programSummaries.get.calibrationObservations,
              selectObservationAndTarget(props.expandedIds),
              selectedTargetIds,
              props.focusedSummaryTargetId,
              focusTargetId,
              _
            ),
            (s, _) => TargetSummaryTitle(props.programId, props.readonly, s)
          )

          val plotData: PlotData =
            PlotData:
              props.focusedAsterismTargetId
                .map(List(_))
                .getOrElse(selectedTargetIds.get)
                .flatMap: targetId =>
                  props.targets.get
                    .get(targetId)
                    .map: target =>
                      ObjectPlotData.Id(targetId.asRight) -> ObjectPlotData(
                        target.name,
                        ObjectTracking.fromTarget(target),
                        props.sitesForTarget(targetId)
                      )
                .toMap

          /**
           * Render the asterism editor
           *
           * @param idsToEdit
           *   The observations to include in the edit. This needs to be a subset of the ids in
           *   asterismGroup
           * @param asterismGroup
           *   The AsterismGroup that is the basis for editing. All or part of it may be included in
           *   the edit.
           */
          def renderAsterismEditor(
            resize:        UseResizeDetectorReturn,
            idsToEdit:     ObsIdSet,
            asterismGroup: AsterismGroup
          ): List[Tile[?]] = {
            val getObsTime: ProgramSummaries => Option[Instant] = a =>
              for
                id <- idsToEdit.single
                o  <- a.observations.getValue(id)
                t  <- o.observationTime
              yield t

            def modObsTime(
              mod: Option[Instant] => Option[Instant]
            ): ProgramSummaries => ProgramSummaries = ps =>
              idsToEdit.single
                .map: i =>
                  ProgramSummaries.observations
                    .filterIndex((id: Observation.Id) => id === i)
                    .andThen(KeyedIndexedList.value)
                    .andThen(Observation.observationTime)
                    .modify(mod)(ps)
                .getOrElse(ps)

            val obsTimeView: View[Option[Instant]] =
              props.programSummaries.model.zoom(getObsTime)(modObsTime)

            val getObsDuration: ProgramSummaries => Option[TimeSpan] = a =>
              for
                id <- idsToEdit.single
                o  <- a.observations.getValue(id)
                t  <- o.observationDuration
              yield t

            def modObsDuration(
              mod: Option[TimeSpan] => Option[TimeSpan]
            ): ProgramSummaries => ProgramSummaries = ps =>
              idsToEdit.single
                .map: i =>
                  ProgramSummaries.observations
                    .filterIndex((id: Observation.Id) => id === i)
                    .andThen(KeyedIndexedList.value)
                    .andThen(Observation.observationDuration)
                    .modify(mod)(ps)
                .getOrElse(ps)

            val obsDurationView: View[Option[TimeSpan]] =
              props.programSummaries.model.zoom(getObsDuration)(modObsDuration)

            val title = idsToEdit.single match {
              case Some(id) => s"Observation $id"
              case None     => s"Editing ${idsToEdit.size} Asterisms"
            }

            val obsConf = idsToEdit.single match {
              case Some(id) =>
                props.programSummaries.get.observations.toList
                  .collect:
                    case o @ Observation(
                          obsId,
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
                          _,
                          posAngle,
                          Some(wavelength),
                          _,
                          _,
                          _,
                          _,
                          _
                        ) if obsId === id =>
                      (const, conf.toBasicConfiguration, posAngle, wavelength, o.needsAGS)
                  .headOption
              case _        => None
            }

            val constraints                               = obsConf.map(_._1)
            val configuration: Option[BasicConfiguration] = obsConf.map(_._2)
            val wavelength                                = obsConf.map(_._4)
            val needsAGS                                  = obsConf.exists(_._5)

            def setCurrentTarget(oids: Option[ObsIdSet])(
              tid: Option[Target.Id],
              via: SetRouteVia
            ): Callback =
              ctx.setPageVia(AppTab.Targets, props.programId, Focused(oids, tid), via)

            def onCloneTarget4Asterism(params: OnCloneParameters): Callback =
              // props.programSummaries.get will always contain the original groups. On creating,
              val allOriginalGroups = props.programSummaries.get.asterismGroups
                .filterForObsInSet(params.obsIds)
                .map(_._1)
                .toSet
              selectedTargetIds.set(List(params.idToAdd)) >>
                (if (params.areCreating) {
                   val obsIds4Url =
                     ObsIdSet.fromSortedSet(idsToEdit.idSet.intersect(params.obsIds.idSet))
                   // all of the original groups that have any of the cloned ids
                   // Deal with the expanded groups - we'll open all affected groups
                   allOriginalGroups.toList.traverse { ids =>
                     val intersect = ids.idSet.intersect(params.obsIds.idSet)
                     if (intersect === ids.idSet.toSortedSet)
                       // it is the whole group, so make sure it is open
                       props.expandedIds.mod(_ + ids)
                     else
                       // otherwise, close the original and open the subsets
                       ObsIdSet
                         .fromSortedSet(intersect)
                         .foldMap(i => props.expandedIds.mod(_ - ids + i + ids.removeUnsafe(i)))
                   }.void >>
                     setCurrentTarget(obsIds4Url)(params.cloneId.some, SetRouteVia.HistoryReplace)
                 } else {
                   // We'll open all of the original groups who had any observations affected by the cloning.
                   props.expandedIds.mod(_ ++ SortedSet.from(allOriginalGroups)) >>
                     setCurrentTarget(idsToEdit.some)(
                       params.originalId.some,
                       SetRouteVia.HistoryReplace
                     )
                 })

            def onAsterismUpdate(params: OnAsterismUpdateParams): Callback =
              val originalGroups = props.programSummaries.get.asterismGroups
              // props.programSummaries.get will always contain the original groups, so we should find the group
              originalGroups
                .findContainingObsIds(params.obsIds)
                .foldMap(group =>
                  val newAsterism                      =
                    if (params.isAddAction) group.targetIds + params.targetId
                    else group.targetIds - params.targetId
                  val existingGroup                    = originalGroups.findWithTargetIds(newAsterism)
                  val mergedObs                        = existingGroup.map(_.obsIds ++ params.obsIds)
                  val obsIdsAfterAction                = mergedObs.getOrElse(params.obsIds)
                  val unmodified                       = group.obsIds -- params.obsIds
                  val setExpanded                      = unmodified.fold {
                    if (params.isUndo) props.expandedIds.mod(_ - obsIdsAfterAction + group.obsIds)
                    else props.expandedIds.mod(_ - group.obsIds + obsIdsAfterAction)
                  }(unmod =>
                    if (params.isUndo)
                      props.expandedIds.mod(_ - obsIdsAfterAction - unmod + group.obsIds)
                    else props.expandedIds.mod(_ - group.obsIds + obsIdsAfterAction + unmod)
                  )
                  val targetForPage: Option[Target.Id] =
                    if (params.areAddingTarget) params.targetId.some
                    else none // if we're deleting, let UI focus the first one in the asterism
                  val setPage: Callback =
                    if (params.isUndo)
                      setCurrentTarget(idsToEdit.some)(targetForPage, SetRouteVia.HistoryReplace)
                    else
                      setCurrentTarget(params.obsIds.some)(
                        targetForPage,
                        SetRouteVia.HistoryReplace
                      )
                  setExpanded >> setPage
                )

            val asterismEditorTile =
              AsterismEditorTile(
                props.userId,
                TargetTabTileIds.AsterismEditor.id,
                props.programId,
                idsToEdit,
                props.obsAndTargets,
                configuration,
                obsTimeView,
                obsDurationView,
                ObsConfiguration(
                  configuration,
                  none,
                  constraints,
                  wavelength,
                  none,
                  none,
                  none,
                  none,
                  needsAGS,
                  none,
                  none
                ),
                none,
                props.focused.target,
                setCurrentTarget(idsToEdit.some),
                onCloneTarget4Asterism,
                onAsterismUpdate,
                getObsInfo(idsToEdit.some),
                props.searching,
                title,
                props.globalPreferences,
                guideStarSelection,
                props.readonly,
                backButton = backButton.some
              )

            val skyPlotTile: Tile[?] =
              ElevationPlotTile.elevationPlotTile(
                props.userId,
                TargetTabTileIds.ElevationPlot.id,
                plotData,
                configuration.map(_.siteFor),
                obsTimeView.get,
                none,
                Nil,
                props.globalPreferences.get,
                "No target selected"
              )

            List(asterismEditorTile, skyPlotTile)
          }

          // We still want to render these 2 tiles, even when not shown, so as not to mess up the stored layout.
          val dummyTargetTile: Tile[Unit]    =
            Tile(TargetTabTileIds.AsterismEditor.id, "", hidden = true)(_ => EmptyVdom)
          val dummyElevationTile: Tile[Unit] =
            Tile(TargetTabTileIds.ElevationPlot.id, "", hidden = true)(_ => EmptyVdom)

          /**
           * Renders a single sidereal target editor without an obs context
           */
          def renderSiderealTargetEditor(
            resize:   UseResizeDetectorReturn,
            targetId: Target.Id
          ): Option[Tile[?]] = {
            def onCloneTarget4Target(params: OnCloneParameters): Callback =
              // It's not perfect, but we'll go to whatever url has the "new" id. This means
              // that if the user went elsewhere before doing undo/redo, they will go back to the new target.
              selectedTargetIds.set(List(params.idToAdd)) >>
                ctx.replacePage(AppTab.Targets, props.programId, Focused.target(params.idToAdd))

            props.targets
              .zoom(Iso.id[TargetList].index(targetId).andThen(Target.sidereal))
              .map: target =>
                SiderealTargetEditorTile.noObsSiderealTargetEditorTile(
                  props.programId,
                  props.userId,
                  targetId,
                  target,
                  props.obsAndTargets,
                  props.searching,
                  s"Editing Target ${target.get.name.value} [$targetId]",
                  fullScreen,
                  props.globalPreferences,
                  guideStarSelection,
                  props.readonly,
                  getObsInfo(none)(targetId),
                  onCloneTarget4Target
                )
          }

          val skyPlotTile: Tile[?] =
            ElevationPlotTile.elevationPlotTile(
              props.userId,
              TargetTabTileIds.ElevationPlot.id,
              plotData,
              selectedTargetIds.get.headOption.flatMap(props.sitesForTarget(_).headOption),
              none,
              none,
              Nil,
              props.globalPreferences.get,
              "No target selected"
            )

          val rightSide = { (resize: UseResizeDetectorReturn) =>
            val observationSetTargetEditorTile
              : Option[List[Tile[?]]] = // Observations selected on tree
              props.focusedIds
                .flatMap(_.toOption)
                .flatMap: obsIds =>
                  findAsterismGroup(obsIds, props.programSummaries.get.asterismGroups)
                    .map: asterismGroup =>
                      renderAsterismEditor(resize, obsIds, asterismGroup)

            val singleTargetEditorTile: Option[Tile[?]] = // Target selected on summary table
              props.focusedSummaryTargetId
                .map:
                  renderSiderealTargetEditor(resize, _)
                .flatten

            val selectedTargetsTiles: List[Tile[?]] =
              List(
                renderSummary.withFullSize,
                singleTargetEditorTile.getOrElse(dummyTargetTile),
                Option // Show plot if and only if the editor is hidden.
                  .when(singleTargetEditorTile.isEmpty)(skyPlotTile)
                  .getOrElse(dummyElevationTile)
              )

            val onlySummary: Boolean =
              observationSetTargetEditorTile.isEmpty && singleTargetEditorTile.isEmpty && skyPlotTile.isEmpty

            val (tiles, key) =
              observationSetTargetEditorTile
                .map:
                  (_, TargetTabTileIds.AsterismEditor.id)
                .getOrElse:
                  (selectedTargetsTiles, TargetTabTileIds.Summary.id)

            TileController(
              props.userId,
              resize.width.getOrElse(1),
              ExploreGridLayouts.targets.defaultTargetLayouts,
              props.userPreferences.get.targetTabLayout,
              tiles,
              GridLayoutSection.TargetLayout,
              backButton.some,
              Option.when(onlySummary)(ExploreStyles.SingleTileMaximized),
              storeLayout = !onlySummary
            ).withKey(key.value): VdomNode
            // withKey is required for the controller to clear it's state between the different
            // layouts or it can get into an invalid state.
          }

          React.Fragment(
            if (LinkingInfo.developmentMode)
              FocusedStatus(AppTab.Targets, props.programId, props.focused)
            else EmptyVdom,
            makeOneOrTwoPanels(
              selectedPanelView,
              targetTree,
              rightSide,
              RightSideCardinality.Multi,
              resize
            )
          )
