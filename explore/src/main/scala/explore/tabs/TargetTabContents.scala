// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.tabs

import cats.Order.*
import cats.effect.IO
import cats.syntax.all.*
import crystal.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.*
import explore.components.ColumnSelectorState
import explore.components.FocusedStatus
import explore.components.Tile
import explore.components.TileController
import explore.components.ui.ExploreStyles
import explore.data.KeyedIndexedList
import explore.model.*
import explore.model.AppContext
import explore.model.Observation
import explore.model.OnCloneParameters
import explore.model.TargetEditObsInfo
import explore.model.enums.AppTab
import explore.model.enums.GridLayoutSection
import explore.model.enums.SelectedPanel
import explore.model.syntax.all.*
import explore.observationtree.AsterismGroupObsList
import explore.shortcuts.*
import explore.shortcuts.given
import explore.targets.DeletingTargets
import explore.targets.ObservationPasteAction
import explore.targets.TargetPasteAction
import explore.targets.TargetSummaryBody
import explore.targets.TargetSummaryTileState
import explore.targets.TargetSummaryTitle
import explore.undo.*
import explore.utils.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.math.Coordinates
import lucuma.core.model.CoordinatesAtVizTime
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.Target.Nonsidereal
import lucuma.core.model.Target.Sidereal
import lucuma.core.model.User
import lucuma.core.util.TimeSpan
import lucuma.react.common.*
import lucuma.react.hotkeys.*
import lucuma.react.hotkeys.hooks.*
import lucuma.react.resizeDetector.*
import lucuma.react.resizeDetector.hooks.*
import lucuma.refined.*
import lucuma.schemas.model.*
import lucuma.ui.optics.*
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import monocle.Iso
import queries.schemas.odb.ObsQueries

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
  val targets: UndoSetter[TargetList]                   = programSummaries.zoom(ProgramSummaries.targets)
  val obsAndTargets: UndoSetter[ObservationsAndTargets] =
    programSummaries.zoom((ProgramSummaries.observations, ProgramSummaries.targets).disjointZip)

  val globalPreferences: View[GlobalPreferences] =
    userPreferences.zoom(UserPreferences.globalPreferences)

object TargetTabContents extends TwoPanels:
  private type Props = TargetTabContents

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
      .useLayoutEffectWithDepsBy((props, _, _, _) => props.focused.target):
        (_, _, _, selTargetIds) => _.foldMap(focusedTarget => selTargetIds.set(List(focusedTarget)))
      .useMemoBy((props, _, _, selTargetIds) => (props.focused, selTargetIds.get)): // Selected observations (right) or targets (left)
        (_, _, _, _) =>
          (target, selTargetIds) =>
            target.obsSet
              .map(_.asRight)
              .orElse:
                TargetIdSet.fromTargetIdList(selTargetIds).map(_.asLeft)
      .useState[LocalClipboard](LocalClipboard.Empty) // shadowClipboard (a copy as state)
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
      .useCallbackWithDepsBy((props, _, _, selTargetIds, _, _, _) => // PASTE Action Callback
        (props.programSummaries.get.asterismGroups, props.readonly, selTargetIds.get)
      ): (props, ctx, _, _, _, _, _) =>
        (asterismGroups, readonly, selTargetIds) =>
          import ctx.given

          val selectObsIds: ObsIdSet => IO[Unit] =
            obsIds => ctx.pushPage(AppTab.Targets, props.programId, Focused.obsSet(obsIds)).toAsync

          ExploreClipboard.get
            .flatMap {
              case LocalClipboard.CopiedObservations(id) =>
                val obsAndTargets =
                  props.focused.obsSet
                    // This with some targets on the tree selected
                    .map(i => id.idSet.toList.map((_, asterismGroups.get(i).foldMap(_.toList))))
                    // These are targets on the table
                    .getOrElse {
                      for
                        tid <- selTargetIds
                        oid <- id.idSet.toList
                      yield (oid, List(tid))
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
      .useGlobalHotkeysWithDepsBy((_, _, _, _, _, _, copyCallback, pasteCallback) =>
        (copyCallback, pasteCallback)
      ) { (props, ctx, _, _, _, _, _, _) => (copyCallback, pasteCallback) =>
        val callbacks: ShortcutCallbacks =
          case CopyAlt1 | CopyAlt2   => copyCallback
          case PasteAlt1 | PasteAlt2 => pasteCallback
          case GoToSummary           => ctx.pushPage(AppTab.Targets, props.programId, Focused.None)

        UseHotkeysProps((GoToSummary :: (CopyKeys ::: PasteKeys)).toHotKeys, callbacks)
      }
      .useStateView(AladinFullScreen.Normal) // full screen aladin
      .useResizeDetector() // Measure its size
      .render:
        (
          props,
          ctx,
          selectedView,
          selectedTargetIds,
          selectedIdsOpt,
          shadowClipboard,
          copyCallback,
          pasteCallback,
          fullScreen,
          resize
        ) =>
          import ctx.given

          def getObsInfo(editing: Option[ObsIdSet])(targetId: Target.Id): TargetEditObsInfo =
            TargetEditObsInfo.fromProgramSummaries(targetId, editing, props.programSummaries.get)

          def targetTree(programSummaries: UndoContext[ProgramSummaries]) =
            AsterismGroupObsList(
              props.programId,
              props.focused,
              props.expandedIds,
              programSummaries,
              selectedIdsOpt,
              shadowClipboard.value,
              selectTargetOrSummary,
              selectedTargetIds.set,
              copyCallback,
              pasteCallback,
              props.readonly
            )

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

          def selectTargetOrSummary(oTargetId: Option[Target.Id]): Callback =
            oTargetId.fold(
              selectedView.set(SelectedPanel.Summary) *>
                setPage(Focused.None)
            ): targetId =>
              setPage(Focused.target(targetId))

          val backButton: VdomNode =
            makeBackButton(props.programId, AppTab.Targets, selectedView, ctx)

          /**
           * Render the summary table.
           */
          val renderSummary: Tile[TargetSummaryTileState] = Tile(
            ObsTabTilesIds.TargetSummaryId.id,
            "Target Summary",
            TargetSummaryTileState(Nil, ColumnSelectorState(), DeletingTargets(false)),
            backButton.some
          )(
            TargetSummaryBody(
              props.userId,
              props.programId,
              props.targets.model,
              props.programSummaries.get.targetObservations,
              props.programSummaries.get.calibrationObservations,
              selectObservationAndTarget(props.expandedIds),
              selectTargetOrSummary,
              selectedTargetIds,
              props.programSummaries,
              props.readonly,
              _
            ),
            (s, _) =>
              TargetSummaryTitle(
                props.programId,
                props.targets.model,
                selectTargetOrSummary,
                selectedTargetIds,
                props.programSummaries,
                props.readonly,
                s
              )
          )

          /**
           * Render the asterism editor
           *
           * @param idsToEdit
           *   The observations to include in the edit. This needs to be asubset of the ids in
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
                props.programSummaries.get.observations.values
                  .collect:
                    case Observation(
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
                          _,
                          posAngle,
                          Some(wavelength),
                          _,
                          _,
                          _,
                          _,
                          _
                        ) if obsId === id =>
                      (const, conf.toBasicConfiguration, posAngle, wavelength)
                  .headOption
              case _        => None
            }

            val constraints                               = obsConf.map(_._1)
            val configuration: Option[BasicConfiguration] = obsConf.map(_._2)
            val wavelength                                = obsConf.map(_._4)

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
                       props.expandedIds.mod(
                         _ + ids
                       ) // it is the whole group, so make sure it is open
                     else
                       // otherwise, close the original and open the subsets
                       ObsIdSet
                         .fromSortedSet(intersect)
                         .foldMap(i =>
                           Callback.log(s"Setting expanded ids for $ids subset $i") >>
                             props.expandedIds.mod(_ - ids + i + ids.removeUnsafe(i))
                         )
                   }.void >>
                     setCurrentTarget(obsIds4Url)(params.cloneId.some, SetRouteVia.HistoryReplace)
                 } else {
                   // We'll open all of the original groups who had any observations affected by the cloning.
                   props.expandedIds.mod(_ ++ SortedSet.from(allOriginalGroups)) >>
                     setCurrentTarget(idsToEdit.some)(params.originalId.some,
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
                  val setPage =
                    if (params.isUndo)
                      setCurrentTarget(idsToEdit.some)(targetForPage, SetRouteVia.HistoryReplace)
                    else
                      setCurrentTarget(params.obsIds.some)(targetForPage,
                                                           SetRouteVia.HistoryReplace
                      )
                  setExpanded >> setPage
                )

            val asterismEditorTile =
              AsterismEditorTile.asterismEditorTile(
                props.userId,
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
                props.readonly,
                backButton = backButton.some
              )

            val selectedCoordinates: Option[Coordinates] =
              props.focused.target.flatMap: id =>
                props.targets.get
                  .get(id)
                  .flatMap:
                    case t @ Target.Sidereal(_, _, _, _) =>
                      obsTimeView.get.flatMap(ot => Target.Sidereal.tracking.get(t).at(ot))
                    case _                               => none

            val skyPlotTile =
              ElevationPlotTile.elevationPlotTile(
                props.userId,
                props.focused.target,
                configuration.map(_.siteFor),
                selectedCoordinates.map(CoordinatesAtVizTime(_)),
                obsTimeView.get,
                none,
                Nil,
                props.globalPreferences.get
              )

            List(asterismEditorTile, skyPlotTile)
          }

          /**
           * Renders a single sidereal target editor without an obs context
           */
          def renderSiderealTargetEditor(
            resize:   UseResizeDetectorReturn,
            targetId: Target.Id
          ): List[Tile[?]] = {
            def onCloneTarget4Target(params: OnCloneParameters): Callback =
              // It's not perfect, but we'll go to whatever url has the "new" id. This means
              // that if the user went elsewhere before doing undo/redo, they will go back to the new target.
              selectedTargetIds.set(List(params.idToAdd)) >>
                ctx.replacePage(AppTab.Targets, props.programId, Focused.target(params.idToAdd))

            val targetTiles: List[Tile[?]] =
              props.targets
                .zoom(Iso.id[TargetList].index(targetId).andThen(Target.sidereal))
                .map { target =>
                  val targetTile = SiderealTargetEditorTile.noObsSiderealTargetEditorTile(
                    props.programId,
                    props.userId,
                    targetId,
                    target,
                    props.obsAndTargets,
                    props.searching,
                    s"Editing Target ${target.get.name.value} [$targetId]",
                    fullScreen,
                    props.globalPreferences,
                    props.readonly,
                    getObsInfo(none)(targetId),
                    onCloneTarget4Target
                  )

                  val skyPlotTile =
                    ElevationPlotTile.elevationPlotTile(
                      props.userId,
                      targetId.some,
                      none,
                      CoordinatesAtVizTime(Target.Sidereal.baseCoordinates.get(target.get)).some,
                      none,
                      none,
                      Nil,
                      props.globalPreferences.get
                    )

                  List(targetTile, skyPlotTile)
                }
                .orEmpty

            renderSummary +: targetTiles
          }

          val optSelected: Option[Either[Target.Id, ObsIdSet]] = props.focused match
            case Focused(Some(obsIdSet), _, _)    => obsIdSet.asRight.some
            case Focused(None, Some(targetId), _) => targetId.asLeft.some
            case _                                => none

          // We still want to render these 2 tiles, even when not shown, so as not to mess up the stored layout.
          val dummyTargetTile: Tile[Unit]    =
            Tile(ObsTabTilesIds.TargetId.id, "", hidden = true)(_ => EmptyVdom)
          val dummyElevationTile: Tile[Unit] =
            Tile(ObsTabTilesIds.PlotId.id, "", hidden = true)(_ => EmptyVdom)

          val renderNonSiderealTargetEditor: List[Tile[?]] =
            List(
              renderSummary,
              Tile("nonSiderealTarget".refined, "Non-sidereal target")(_ =>
                <.div("Editing of Non-Sidereal targets not supported")
              ),
              dummyElevationTile
            )

          val rightSide = { (resize: UseResizeDetectorReturn) =>
            val tileListKeyOpt: Option[(List[Tile[?]], NonEmptyString)] =
              optSelected
                .flatMap:
                  _ match
                    case Left(targetId) =>
                      props.targets.get
                        .get(targetId)
                        .map:
                          case Nonsidereal(_, _, _) =>
                            (renderNonSiderealTargetEditor, TargetTabControllerIds.Summary.id)
                          case Sidereal(_, _, _, _) =>
                            (renderSiderealTargetEditor(resize, targetId),
                             TargetTabControllerIds.Summary.id
                            )
                    case Right(obsIds)  =>
                      findAsterismGroup(obsIds, props.programSummaries.get.asterismGroups)
                        .map: asterismGroup =>
                          (renderAsterismEditor(resize, obsIds, asterismGroup),
                           TargetTabControllerIds.AsterismEditor.id
                          )

            val justSummaryTiles = List(
              renderSummary,
              dummyTargetTile,
              dummyElevationTile
            )

            val (tiles, key) =
              tileListKeyOpt.fold((justSummaryTiles, TargetTabControllerIds.Summary.id)):
                (tiles, key) => (tiles, key)

            TileController(
              props.userId,
              resize.width.getOrElse(1),
              ExploreGridLayouts.targets.defaultTargetLayouts,
              props.userPreferences.get.targetTabLayout,
              tiles,
              GridLayoutSection.TargetLayout,
              backButton.some,
              Option.when(tileListKeyOpt.isEmpty)(ExploreStyles.SingleTileMaximized),
              storeLayout = tileListKeyOpt.nonEmpty
            ).withKey(key.value): VdomNode
            // withKey is required for the controller to clear it's state between the different
            // layouts or it can get into an invalid state.
          }

          React.Fragment(
            if (LinkingInfo.developmentMode)
              FocusedStatus(AppTab.Targets, props.programId, props.focused)
            else EmptyVdom,
            makeOneOrTwoPanels(
              selectedView,
              targetTree(props.programSummaries),
              rightSide,
              RightSideCardinality.Multi,
              resize
            )
          )
