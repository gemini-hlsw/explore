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
import explore.model.TargetList
import explore.undo.UndoSetter
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.NewType
import lucuma.react.common.Css
import lucuma.react.common.ReactFnProps
import lucuma.refined.*
import lucuma.schemas.model.TargetWithId
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
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
  allTargets:        UndoSetter[TargetList],
  vizTime:           View[Option[Instant]],
  configuration:     ObsConfiguration,
  focusedTargetId:   Option[Target.Id],
  setTarget:         (Option[Target.Id], SetRouteVia) => Callback,
  otherObsCount:     Target.Id => Int,
  searching:         View[Set[Target.Id]],
  renderInTitle:     Tile.RenderInTitle,
  globalPreferences: View[GlobalPreferences],
  readonly:          Boolean,
  sequenceChanged:   Callback
) extends ReactFnProps(AsterismEditor.component)

object AreAdding extends NewType[Boolean]
type AreAdding = AreAdding.Type

object AsterismEditor extends AsterismModifier:
  private type Props = AsterismEditor

  private object EditScope extends NewType[Boolean]:
    inline def AllInstances: EditScope = EditScope(true)
    inline def CurrentOnly: EditScope  = EditScope(false)

  private type EditScope = EditScope.Type

  private def onCloneTarget(
    asterismIds: View[AsterismIds],
    allTargets:  View[TargetList],
    setTarget:   (Option[Target.Id], SetRouteVia) => Callback
  )(
    newTwid:     TargetWithId
  ): Callback =
    allTargets.mod(_ + (newTwid.id -> newTwid.target)) >>
      asterismIds.mod(_ + newTwid.id) >>
      setTarget(newTwid.id.some, SetRouteVia.HistoryPush)

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView(AreAdding(false))
      .useStateView(EditScope.CurrentOnly)
      .useEffectWithDepsBy((props, _, _, _) => (props.asterismIds.get, props.focusedTargetId)) {
        (props, _, _, _) => (asterismIds, focusedTargetId) =>
          // If the selected targetId is None, or not in the asterism, select the first target (if any).
          // Need to replace history here.
          focusedTargetId.filter(asterismIds.contains_) match
            case None => props.setTarget(asterismIds.headOption, SetRouteVia.HistoryReplace)
            case _    => Callback.empty
      }
      // full screen aladin
      .useStateView(AladinFullScreen.Normal)
      .render { (props, ctx, adding, editScope, fullScreen) =>
        import ctx.given

        val targetView: View[Option[Target.Id]] =
          View[Option[Target.Id]](
            props.focusedTargetId,
            { (f, cb) =>
              val newValue = f(props.focusedTargetId)
              props.setTarget(newValue, SetRouteVia.HistoryPush) >> cb(newValue)
            }
          )

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
              val newValue = mod(props.focusedTargetId)
              props.setTarget(newValue, SetRouteVia.HistoryPush) >> cb(newValue)
          )

        <.div(
          ExploreStyles.AladinFullScreen.when(fullScreen.get.value),
          if (props.readonly) EmptyVdom
          else
            props.renderInTitle(
              targetSelectionPopup(
                "Add",
                props.programId,
                props.obsIds,
                props.asterismIds,
                props.allTargets.model,
                adding,
                targetView.async.set
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

            val otherObsCount = props.otherObsCount(focusedTargetId)
            val plural        = if (otherObsCount === 1) "" else "s"

            selectedTargetOpt
              .map(siderealTarget =>
                <.div(
                  ExploreStyles.TargetTileEditor,
                  <.div(
                    ExploreStyles.SharedEditWarning,
                    s"${siderealTarget.get.name.value} is in ${otherObsCount} other observation$plural. Edits here should apply to:",
                    BooleanRadioButtons(
                      view = editScope.as(EditScope.value),
                      idBase = "editscope".refined,
                      name = "editScope".refined,
                      trueLabel = "all observations of this target".refined,
                      falseLabel =
                        if (props.obsIds.size === 1) "only this observation".refined
                        else "only the current observations".refined,
                    ).toFalseTrueFragment
                  ).when(otherObsCount > 0 && !props.readonly),
                  SiderealTargetEditor(
                    props.userId,
                    siderealTarget,
                    asterism.focusOn(focusedTargetId),
                    vizTime,
                    props.configuration.some,
                    props.searching,
                    onClone = onCloneTarget(
                      props.asterismIds,
                      props.allTargets.model,
                      props.setTarget
                    ),
                    obsIdSubset =
                      if (otherObsCount > 0 && editScope.get === EditScope.CurrentOnly)
                        props.obsIds.some
                      else none,
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
