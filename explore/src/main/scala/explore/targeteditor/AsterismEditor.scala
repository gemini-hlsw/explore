// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all.*
import crystal.react.*
import crystal.react.hooks.*
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.config.VizTimeEditor
import explore.model.AladinFullScreen
import explore.model.AppContext
import explore.model.Asterism
import explore.model.AsterismIds
import explore.model.GlobalPreferences
import explore.model.ObsConfiguration
import explore.model.ObsIdSet
import explore.model.ObservationsAndTargets
import explore.model.OnCloneParameters
import explore.model.TargetEditObsInfo
import explore.model.TargetList
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.NewType
import lucuma.react.common.ReactFnProps
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.given
import monocle.Iso
import queries.schemas.odb.ObsQueries

import java.time.Instant

case class AsterismEditor(
  userId:            User.Id,
  programId:         Program.Id,
  obsIds:            ObsIdSet,
  asterismIds:       View[AsterismIds],
  obsAndTargets:     UndoSetter[ObservationsAndTargets],
  vizTime:           View[Option[Instant]],
  configuration:     ObsConfiguration,
  focusedTargetId:   Option[Target.Id],
  setTarget:         (Option[Target.Id], SetRouteVia) => Callback,
  onCloneTarget:     OnCloneParameters => Callback,
  obsInfo:           Target.Id => TargetEditObsInfo,
  searching:         View[Set[Target.Id]],
  renderInTitle:     Tile.RenderInTitle,
  globalPreferences: View[GlobalPreferences],
  readonly:          Boolean,
  sequenceChanged:   Callback
) extends ReactFnProps(AsterismEditor.component):
  val allTargets: UndoSetter[TargetList] = obsAndTargets.zoom(ObservationsAndTargets.targets)

object AreAdding extends NewType[Boolean]
type AreAdding = AreAdding.Type

object AsterismEditor extends AsterismModifier:
  private type Props = AsterismEditor

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView(AreAdding(false))
      .useEffectWithDepsBy((props, _, _) => (props.asterismIds.get, props.focusedTargetId)) {
        (props, _, _) => (asterismIds, focusedTargetId) =>
          // If the selected targetId is None, or not in the asterism, select the first target (if any).
          // Need to replace history here.
          focusedTargetId.filter(asterismIds.contains_) match
            case None => props.setTarget(asterismIds.headOption, SetRouteVia.HistoryReplace)
            case _    => Callback.empty
      }
      // full screen aladin
      .useStateView(AladinFullScreen.Normal)
      .render { (props, ctx, adding, fullScreen) =>
        import ctx.given

        // Save the time here. this works for the obs and target tabs
        val vizTimeView = props.vizTime.withOnMod(t =>
          ObsQueries
            .updateVisualizationTime[IO](props.obsIds.toList, t)
            .runAsync
        )

        val vizTime = props.vizTime.get

        val selectedTargetView: View[Option[Target.Id]] =
          View(
            props.focusedTargetId,
            (mod, cb) =>
              val oldValue = props.focusedTargetId
              val newValue = mod(props.focusedTargetId)
              props.setTarget(newValue, SetRouteVia.HistoryPush) >> cb(oldValue, newValue)
          )

        <.div(
          ExploreStyles.AladinFullScreen.when(fullScreen.get.value),
          props.renderInTitle(
            targetSelectionPopup(
              "Add",
              props.programId,
              props.obsIds,
              props.asterismIds,
              props.allTargets.model,
              adding,
              selectedTargetView.async.set,
              props.readonly,
              ExploreStyles.AddTargetButton
            )
          ),
          props.renderInTitle(VizTimeEditor(vizTimeView)),
          TargetTable(
            props.userId.some,
            props.programId,
            props.obsIds,
            props.asterismIds,
            props.allTargets.model,
            selectedTargetView,
            vizTime,
            props.renderInTitle,
            fullScreen.get,
            props.readonly
          ),
          // it's possible for us to get here without an asterism but with a focused target id. This will get
          // corrected, but we need to not render the target editor before it is corrected.
          (Asterism.fromIdsAndTargets(props.asterismIds.get, props.allTargets.get),
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
