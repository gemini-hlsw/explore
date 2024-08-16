// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import explore.components.ColumnSelectorInTitle
import explore.components.ColumnSelectorState
import explore.components.ui.ExploreStyles
import explore.config.ObsTimeEditor
import explore.model.AladinFullScreen
import explore.model.AppContext
import explore.model.Asterism
import explore.model.GlobalPreferences
import explore.model.ObsConfiguration
import explore.model.ObsIdSet
import explore.model.ObsIdSetEditInfo
import explore.model.ObservationsAndTargets
import explore.model.OnAsterismUpdateParams
import explore.model.OnCloneParameters
import explore.model.TargetEditObsInfo
import explore.model.TargetList
import explore.model.reusability.given
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.NewType
import lucuma.react.common.ReactFnProps
import lucuma.schemas.model.SiderealTargetWithId
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import monocle.Focus
import monocle.Iso
import monocle.Lens

import java.time.Instant

case class AsterismTileState(
  table:       ColumnSelectorState[SiderealTargetWithId, TargetTable.TableMeta] =
    ColumnSelectorState[SiderealTargetWithId, TargetTable.TableMeta](),
  obsEditInfo: Option[ObsIdSetEditInfo] = None
) {
  // the 'getOrElse doesn't matter. Controls will be readonly if all are executed
  def unexecutedObs(obsIds: ObsIdSet): Option[ObsIdSet] =
    obsEditInfo.map(_.unExecuted.getOrElse(obsIds))
}

object AsterismTileState:
  val table
    : Lens[AsterismTileState, ColumnSelectorState[SiderealTargetWithId, TargetTable.TableMeta]] =
    Focus[AsterismTileState](_.table)
  val obsEditInfo: Lens[AsterismTileState, Option[ObsIdSetEditInfo]] =
    Focus[AsterismTileState](_.obsEditInfo)

case class AsterismEditorBody(
  programId:         Program.Id,
  userId:            User.Id,
  obsIds:            ObsIdSet,
  obsAndTargets:     UndoSetter[ObservationsAndTargets],
  vizTime:           View[Option[Instant]],
  configuration:     ObsConfiguration,
  focusedTargetId:   Option[Target.Id],
  setTarget:         (Option[Target.Id], SetRouteVia) => Callback,
  onCloneTarget:     OnCloneParameters => Callback,
  onAsterismUpdate:  OnAsterismUpdateParams => Callback,
  obsInfo:           Target.Id => TargetEditObsInfo,
  searching:         View[Set[Target.Id]],
  globalPreferences: View[GlobalPreferences],
  readonly:          Boolean,
  sequenceChanged:   Callback,
  tileState:         View[AsterismTileState]
) extends ReactFnProps(AsterismEditorBody.component):
  val allTargets: UndoSetter[TargetList] = obsAndTargets.zoom(ObservationsAndTargets.targets)

object AreAdding extends NewType[Boolean]
type AreAdding = AreAdding.Type

object AsterismEditorBody extends AsterismModifier:
  private type Props = AsterismEditorBody

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useMemoBy(props => (props.obsIds, props.obsAndTargets.get._1)) { _ =>
        ObsIdSetEditInfo.fromObservationList
      }
      .useLayoutEffectWithDepsBy((_, obsEditInfo) => obsEditInfo) { (p, _) => obsEditInfo =>
        p.tileState.zoom(AsterismTileState.obsEditInfo).set(obsEditInfo.value.some)
      }
      .useLayoutEffectWithDepsBy((props, obsEditInfo) =>
        (obsEditInfo.asterismIds, props.focusedTargetId)
      ) { (props, _) => (asterismIds, focusedTargetId) =>
        // If the selected targetId is None, or not in the asterism, select the first target (if any).
        // Need to replace history here.
        focusedTargetId.filter(asterismIds.contains_) match
          case None => props.setTarget(asterismIds.headOption, SetRouteVia.HistoryReplace)
          case _    => Callback.empty
      }
      // full screen aladin
      .useStateView(AladinFullScreen.Normal)
      .render { (props, obsEditInfo, fullScreen) =>

        val vizTime = props.vizTime.get

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

        <.div(
          ExploreStyles.AladinFullScreen.when(fullScreen.get.value),
          editWarningMsg.map(msg => <.div(ExploreStyles.SharedEditWarning, msg)),
          props.tileState.get
            .unexecutedObs(props.obsIds)
            .map(unexecutedObs =>
              TargetTable(
                props.userId.some,
                props.programId,
                unexecutedObs,
                obsEditInfo.asterismIds,
                props.obsAndTargets,
                selectedTargetView,
                props.onAsterismUpdate,
                vizTime,
                fullScreen.get,
                props.readonly || obsEditInfo.allAreExecuted
              )(props.tileState.zoom(AsterismTileState.table))
            ),
          // it's possible for us to get here without an asterism but with a focused target id. This will get
          // corrected, but we need to not render the target editor before it is corrected.
          (Asterism.fromIdsAndTargets(obsEditInfo.asterismIds, props.allTargets.get),
           props.focusedTargetId
          ).mapN { (asterism, focusedTargetId) =>
            val selectedTargetOpt: Option[UndoSetter[Target.Sidereal]] =
              props.allTargets
                .zoom(Iso.id[TargetList].index(focusedTargetId).andThen(Target.sidereal))

            val obsInfo = props.obsInfo(focusedTargetId)

            selectedTargetOpt
              .map(siderealTarget =>
                <.div(
                  ExploreStyles.TargetTileEditor,
                  SiderealTargetEditor(
                    props.programId,
                    props.userId,
                    siderealTarget,
                    props.obsAndTargets,
                    asterism.focusOn(focusedTargetId),
                    vizTime,
                    props.configuration.some,
                    props.searching,
                    onClone = props.onCloneTarget,
                    obsInfo = obsInfo,
                    fullScreen = fullScreen,
                    globalPreferences = props.globalPreferences,
                    readonly = props.readonly,
                    invalidateSequence = props.sequenceChanged
                  )
                )
              )
              .getOrElse[VdomElement](
                <.div("Non-sidereal targets not supported")
              )
          }
        )
      }

case class AsterismEditorTitle(
  programId:        Program.Id,
  obsIds:           ObsIdSet,
  obsAndTargets:    UndoSetter[ObservationsAndTargets],
  onAsterismUpdate: OnAsterismUpdateParams => Callback,
  readonly:         Boolean,
  vizTimeView:      View[Option[Instant]],
  tileState:        View[AsterismTileState]
) extends ReactFnProps(AsterismEditorTitle.component)

object AsterismEditorTitle extends AsterismModifier:
  private type Props = AsterismEditorTitle

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView(AreAdding(false))
      .render: (props, ctx, adding) =>
        import ctx.given

        React.Fragment(
          // only pass in the unexecuted observations. Will be readonly if there aren't any
          (props.tileState.get.obsEditInfo,
           props.tileState.get
             .unexecutedObs(props.obsIds)
          ).mapN((obsEditInfo, unexecutedObs) =>
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
          ObsTimeEditor(props.vizTimeView),
          ColumnSelectorInTitle(TargetTable.columnNames,
                                props.tileState.zoom(AsterismTileState.table)
          )
        )
