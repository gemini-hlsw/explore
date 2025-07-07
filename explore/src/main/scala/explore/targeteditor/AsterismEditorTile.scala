// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import eu.timepit.refined.types.string.NonEmptyString
import explore.components.ColumnSelectorInTitle
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.config.ObsTimeEditor
import explore.model.AladinFullScreen
import explore.model.AppContext
import explore.model.Asterism
import explore.model.AttachmentList
import explore.model.GlobalPreferences
import explore.model.GuideStarSelection
import explore.model.ObsConfiguration
import explore.model.ObsIdSet
import explore.model.ObsIdSetEditInfo
import explore.model.ObservationsAndTargets
import explore.model.OnAsterismUpdateParams
import explore.model.OnCloneParameters
import explore.model.TargetEditObsInfo
import explore.model.TargetList
import explore.model.enums.TileSizeState
import explore.model.reusability.given
import explore.services.OdbObservationApi
import explore.targets.TargetColumns
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.sequence.ExecutionDigest
import lucuma.core.util.CalculatedValue
import lucuma.core.util.TimeSpan
import lucuma.react.common.ReactFnProps
import lucuma.react.table.ColumnVisibility
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import monocle.Focus
import monocle.Iso
import monocle.Lens
import org.typelevel.log4cats.Logger

import java.time.Instant

object AsterismEditorTile:
  def apply(
    userId:             Option[User.Id],
    tileId:             Tile.TileId,
    programId:          Program.Id,
    obsIds:             ObsIdSet,
    obsAndTargets:      UndoSetter[ObservationsAndTargets],
    obsTime:            View[Option[Instant]],
    obsDuration:        View[Option[TimeSpan]],
    obsConf:            ObsConfiguration,
    digest:             CalculatedValue[Option[ExecutionDigest]],
    currentTarget:      Option[Target.Id],
    setTarget:          (Option[Target.Id], SetRouteVia) => Callback,
    onCloneTarget:      OnCloneParameters => Callback,
    onAsterismUpdate:   OnAsterismUpdateParams => Callback,
    obsInfo:            Target.Id => TargetEditObsInfo,
    searching:          View[Set[Target.Id]],
    title:              String,
    globalPreferences:  View[GlobalPreferences],
    guideStarSelection: View[GuideStarSelection],
    attachments:        View[AttachmentList],
    authToken:          Option[NonEmptyString],
    readonly:           Boolean,
    sequenceChanged:    Callback = Callback.empty,
    backButton:         Option[VdomNode] = None
  )(using odbApi: OdbObservationApi[IO])(using Logger[IO]): Tile[TileState] = {
    // Save the time here. this works for the obs and target tabs
    // It's OK to save the viz time for executed observations, I think.
    val obsTimeView: View[Option[Instant]] =
      obsTime.withOnMod(t => odbApi.updateVisualizationTime(obsIds.toList, t).runAsync)

    val obsDurationView: View[Option[TimeSpan]] =
      obsDuration.withOnMod: t =>
        odbApi.updateVisualizationDuration(obsIds.toList, t).runAsync

    val obsTimeAndDurationView: View[(Option[Instant], Option[TimeSpan])] =
      View(
        (obsTime.get, obsDuration.get),
        (mod, cb) =>
          val oldValue = (obsTime.get, obsDuration.get)
          val newValue = mod(oldValue)
          obsTime.set(newValue._1) >> obsDuration.set(newValue._2) >> cb(oldValue, newValue)
      ).withOnMod: tuple =>
        odbApi
          .updateVisualizationTimeAndDuration(obsIds.toList, tuple._1, tuple._2)
          .runAsync

    Tile(
      tileId,
      title,
      TileState.Initial,
      back = backButton,
      bodyClass = ExploreStyles.TargetTileBody,
      controllerClass = ExploreStyles.TargetTileController
    )(
      tileState =>
        userId.map: uid =>
          Body(
            programId,
            uid,
            obsIds,
            obsAndTargets,
            obsTime.get,
            obsConf,
            currentTarget,
            setTarget,
            onCloneTarget,
            onAsterismUpdate,
            obsInfo,
            searching,
            globalPreferences,
            guideStarSelection,
            attachments,
            authToken,
            readonly,
            sequenceChanged,
            tileState.zoom(TileState.columnVisibility),
            tileState.zoom(TileState.obsEditInfo)
          ),
      (tileState, tileSize) =>
        Title(
          programId,
          obsIds,
          obsAndTargets,
          onAsterismUpdate,
          readonly,
          obsTimeView,
          obsDurationView,
          obsTimeAndDurationView,
          digest,
          tileState.zoom(TileState.columnVisibility),
          tileState.get.obsEditInfo,
          tileSize
        )
    )
  }

  case class TileState(
    columnVisibility: ColumnVisibility,
    obsEditInfo:      Option[ObsIdSetEditInfo]
  )

  object TileState:
    val Initial: TileState = TileState(TargetColumns.DefaultVisibility, none)

    val columnVisibility: Lens[TileState, ColumnVisibility]    =
      Focus[TileState](_.columnVisibility)
    val obsEditInfo: Lens[TileState, Option[ObsIdSetEditInfo]] =
      Focus[TileState](_.obsEditInfo)

  private case class Body(
    programId:          Program.Id,
    userId:             User.Id,
    obsIds:             ObsIdSet,
    obsAndTargets:      UndoSetter[ObservationsAndTargets],
    obsTime:            Option[Instant],
    obsConf:            ObsConfiguration,
    focusedTargetId:    Option[Target.Id],
    setTarget:          (Option[Target.Id], SetRouteVia) => Callback,
    onCloneTarget:      OnCloneParameters => Callback,
    onAsterismUpdate:   OnAsterismUpdateParams => Callback,
    obsInfo:            Target.Id => TargetEditObsInfo,
    searching:          View[Set[Target.Id]],
    globalPreferences:  View[GlobalPreferences],
    guideStarSelection: View[GuideStarSelection],
    attachments:        View[AttachmentList],
    authToken:          Option[NonEmptyString],
    readonly:           Boolean,
    sequenceChanged:    Callback,
    columnVisibility:   View[ColumnVisibility],
    obsEditInfo:        View[Option[ObsIdSetEditInfo]]
  ) extends ReactFnProps(Body.component):
    val allTargets: UndoSetter[TargetList] = obsAndTargets.zoom(ObservationsAndTargets.targets)

  private object Body extends AsterismModifier:
    private type Props = Body

    private val component =
      ScalaFnComponent
        .withHooks[Props]
        .useMemoBy(props => (props.obsIds, props.obsAndTargets.get._1)): _ =>
          ObsIdSetEditInfo.fromObservationList
        .useLayoutEffectWithDepsBy((_, obsEditInfo) => obsEditInfo): (props, _) =>
          obsEditInfo => props.obsEditInfo.set(obsEditInfo.value.some)
        .useLayoutEffectWithDepsBy((props, obsEditInfo) =>
          (obsEditInfo.asterismIds, props.focusedTargetId)
        ): (props, _) =>
          (asterismIds, focusedTargetId) =>
            // If the selected targetId is None, or not in the asterism, select the first target (if any).
            // Need to replace history here.
            focusedTargetId.filter(asterismIds.contains_) match
              case None => props.setTarget(asterismIds.headOption, SetRouteVia.HistoryReplace)
              case _    => Callback.empty
        // full screen aladin
        .useStateView(AladinFullScreen.Normal)
        .render: (props, obsEditInfo, fullScreen) =>
          val selectedTargetView: View[Option[Target.Id]] =
            View(
              props.focusedTargetId,
              (mod, cb) =>
                val oldValue = props.focusedTargetId
                val newValue = mod(props.focusedTargetId)
                props.setTarget(newValue, SetRouteVia.HistoryPush) >> cb(oldValue, newValue)
            )

          val editWarningMsg: Option[String] =
            if (obsEditInfo.allAreExecuted)
              if (obsEditInfo.editing.length > 1)
                "All of the current observations are executed. Asterism is readonly.".some
              else "The current observation has been executed. Asterism is readonly.".some
            else if (obsEditInfo.executed.isDefined)
              "Adding and removing targets will only affect the unexecuted observations.".some
            else none

          <.div(ExploreStyles.AladinFullScreen.when(fullScreen.get.value))(
            editWarningMsg.map(msg => <.div(ExploreStyles.SharedEditWarning, msg)),
            // the 'getOrElse doesn't matter. Controls will be readonly if all are executed
            props.obsEditInfo.get
              .map(_.unExecuted.getOrElse(props.obsIds))
              .map: unexecutedObs =>
                TargetTable(
                  props.userId.some,
                  props.programId,
                  unexecutedObs,
                  obsEditInfo.asterismIds,
                  props.obsAndTargets,
                  selectedTargetView,
                  props.onAsterismUpdate,
                  props.obsTime,
                  fullScreen.get,
                  props.readonly || obsEditInfo.allAreExecuted,
                  props.columnVisibility
                ),
            // it's possible for us to get here without an asterism but with a focused target id. This will get
            // corrected, but we need to not render the target editor before it is corrected.
            (Asterism.fromIdsAndTargets(obsEditInfo.asterismIds, props.allTargets.get),
             props.focusedTargetId
            ).mapN: (asterism, focusedTargetId) =>
              val selectedTargetOpt: Option[UndoSetter[Target.Sidereal]] =
                props.allTargets
                  .zoom(Iso.id[TargetList].index(focusedTargetId).andThen(Target.sidereal))

              val obsInfo = props.obsInfo(focusedTargetId)

              selectedTargetOpt
                .map: siderealTarget =>
                  <.div(
                    ExploreStyles.TargetTileEditor,
                    SiderealTargetEditor(
                      props.programId,
                      props.userId,
                      siderealTarget,
                      props.obsAndTargets,
                      asterism.focusOn(focusedTargetId),
                      props.obsTime,
                      props.obsConf.some,
                      props.searching,
                      onClone = props.onCloneTarget,
                      obsInfo = obsInfo,
                      fullScreen = fullScreen,
                      globalPreferences = props.globalPreferences,
                      guideStarSelection = props.guideStarSelection,
                      attachments = props.attachments,
                      authToken = props.authToken,
                      readonly = props.readonly,
                      invalidateSequence = props.sequenceChanged
                    )
                  )
                .getOrElse[VdomElement]:
                  <.div("Non-sidereal targets not supported")
          )

  private case class Title(
    programId:              Program.Id,
    obsIds:                 ObsIdSet,
    obsAndTargets:          UndoSetter[ObservationsAndTargets],
    onAsterismUpdate:       OnAsterismUpdateParams => Callback,
    readonly:               Boolean,
    obsTimeView:            View[Option[Instant]],
    obsDurationView:        View[Option[TimeSpan]],
    obsTimeAndDurationView: View[(Option[Instant], Option[TimeSpan])],
    digest:                 CalculatedValue[Option[ExecutionDigest]],
    columnVisibility:       View[ColumnVisibility],
    obsEditInfo:            Option[ObsIdSetEditInfo],
    tileSize:               TileSizeState
  ) extends ReactFnProps(Title.component)

  private object Title extends AsterismModifier:
    private type Props = Title

    private val component =
      ScalaFnComponent[Props]: props =>
        for
          ctx    <- useContext(AppContext.ctx)
          adding <- useStateView(AreAdding(false))
        yield
          import ctx.given

          val obsTimeEditor = ObsTimeEditor(
            props.obsTimeView,
            props.obsDurationView,
            props.obsTimeAndDurationView,
            props.digest,
            props.obsIds.size > 1
          )

          <.div(
            ExploreStyles.AsterismEditorTileTitle,
            if (props.tileSize.isMinimized)
              obsTimeEditor
            else
              React.Fragment(
                // only pass in the unexecuted observations. Will be readonly if there aren't any
                <.span(
                  (props.obsEditInfo, props.obsEditInfo.map(_.unExecuted.getOrElse(props.obsIds)))
                    .mapN: (obsEditInfo, unexecutedObs) =>
                      targetSelectionPopup(
                        "Add",
                        props.programId,
                        unexecutedObs,
                        props.obsAndTargets,
                        adding,
                        props.onAsterismUpdate,
                        props.readonly || obsEditInfo.allAreExecuted,
                        ExploreStyles.AddTargetButton
                      )
                ),
                obsTimeEditor,
                <.span(^.textAlign.right)(
                  ColumnSelectorInTitle(TargetColumns.AllColNames.toList, props.columnVisibility)
                )
              )
          )
