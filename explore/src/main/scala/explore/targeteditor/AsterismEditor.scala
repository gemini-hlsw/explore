// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import crystal.Pot
import crystal.react.View
import crystal.react.hooks.*
import crystal.react.implicits.*
import explore.Icons
import explore.common.AsterismQueries
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.config.VizTimeEditor
import explore.model.AladinFullScreen
import explore.model.AppContext
import explore.model.Asterism
import explore.model.Asterism.siderealTargetsEach
import explore.model.AsterismIds
import explore.model.ObsConfiguration
import explore.model.ObsIdSet
import explore.model.PAProperties
import explore.model.TargetList
import explore.model.enums.AgsState
import explore.model.reusability.given
import explore.model.reusability.given
import explore.optics.*
import explore.optics.all.*
import explore.syntax.ui.*
import explore.targets.TargetSelectionPopup
import explore.targets.TargetSource
import explore.undo.UndoStacks
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.util.DefaultEffects.{Sync => DefaultS}
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.data.Zipper
import lucuma.core.math.Angle
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.Observation
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.NewType
import lucuma.refined.*
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.*
import lucuma.schemas.odb.input.*
import lucuma.ui.primereact.*
import lucuma.ui.primereact.given
import lucuma.ui.reusability.given
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import monocle.Iso
import monocle.Lens
import monocle.Optional
import monocle.Traversal
import monocle.std.option.some
import org.typelevel.log4cats.Logger
import queries.common.TargetQueriesGQL.*
import queries.schemas.odb.ObsQueries
import react.common.ReactFnProps
import react.primereact.Button

import java.time.Instant

case class AsterismEditor(
  userId:          User.Id,
  programId:       Program.Id,
  obsIds:          ObsIdSet,
  asterismIds:     View[AsterismIds],
  allTargets:      View[TargetList],
  vizTime:         View[Option[Instant]],
  configuration:   ObsConfiguration,
  focusedTargetId: Option[Target.Id],
  setTarget:       (Option[Target.Id], SetRouteVia) => Callback,
  otherObsCount:   Target.Id => Int,
  undoStacks:      View[Map[Target.Id, UndoStacks[IO, Target.Sidereal]]],
  searching:       View[Set[Target.Id]],
  renderInTitle:   Tile.RenderInTitle
) extends ReactFnProps(AsterismEditor.component)

object AsterismEditor extends AsterismModifier:
  private type Props = AsterismEditor

  private object EditScope extends NewType[Boolean]:
    inline def AllInstances: EditScope = EditScope(true)
    inline def CurrentOnly: EditScope  = EditScope(false)

  private type EditScope = EditScope.Type

  private object AreAdding extends NewType[Boolean]
  private type AreAdding = AreAdding.Type

  private def onCloneTarget(
    id:          Target.Id,
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
            .updateVisualizationTime[IO](props.programId, props.obsIds.toList, t)
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
          props.renderInTitle(
            TargetSelectionPopup(
              "Add Target",
              TargetSource.FromProgram[IO](props.programId) :: TargetSource.forAllCatalogs[IO],
              selectExistingLabel = "Link",
              selectExistingIcon = Icons.Link,
              selectNewLabel = "Add",
              selectNewIcon = Icons.New,
              trigger = Button(
                severity = Button.Severity.Success,
                disabled = adding.get.value,
                icon = Icons.New,
                loading = adding.get.value,
                label = "Add"
              ).tiny.compact,
              onSelected = targetWithOptId =>
                insertSiderealTarget(
                  props.programId,
                  props.obsIds,
                  props.asterismIds,
                  props.allTargets,
                  targetWithOptId
                ).flatMap(oTargetId => targetView.async.set(oTargetId))
                  .switching(adding.async, AreAdding(_))
                  .runAsync
            )
          ),
          props.renderInTitle(VizTimeEditor(vizTimeView)),
          TargetTable(
            props.userId.some,
            props.programId,
            props.obsIds,
            props.asterismIds,
            props.allTargets.get,
            selectedTargetView,
            vizTime,
            props.renderInTitle,
            fullScreen.get
          ),
          props.focusedTargetId.map { focusedTargetId =>
            val selectedTargetView =
              props.allTargets.zoom(Iso.id[TargetList].index(focusedTargetId))

            val otherObsCount = props.otherObsCount(focusedTargetId)
            val plural        = if (otherObsCount === 1) "" else "s"

            selectedTargetView
              .zoom(Target.sidereal)
              .mapValue[VdomElement](siderealTargetView =>
                <.div(
                  ExploreStyles.TargetTileEditor,
                  <.div(
                    ExploreStyles.SharedEditWarning,
                    s"${siderealTargetView.get.name.value} is in ${otherObsCount} other observation$plural. Edits here should apply to:",
                    BooleanRadioButtons(
                      view = editScope.as(EditScope.value),
                      idBase = "editscope".refined,
                      name = "editScope".refined,
                      trueLabel = "all observations of this target".refined,
                      falseLabel =
                        if (props.obsIds.size === 1) "only this observation".refined
                        else "only the current observations".refined,
                    ).toFalseTrueFragment
                  ).when(otherObsCount > 0),
                  SiderealTargetEditor(
                    props.userId,
                    focusedTargetId,
                    siderealTargetView,
                    Asterism
                      .fromIdsAndTargets(props.asterismIds.get, props.allTargets.get)
                      .map(_.focusOn(focusedTargetId)),
                    vizTime,
                    props.configuration.some,
                    props.undoStacks
                      .zoom(atMapWithDefault(focusedTargetId, UndoStacks.empty)),
                    props.searching,
                    onClone = onCloneTarget(
                      focusedTargetId,
                      props.asterismIds,
                      props.allTargets,
                      props.setTarget
                    ) _,
                    obsIdSubset =
                      if (otherObsCount > 0 && editScope.get === EditScope.CurrentOnly)
                        props.obsIds.some
                      else none,
                    fullScreen = fullScreen
                  )
                )
              )
              .getOrElse[VdomElement](
                <.div("Non-sidereal targets not supported")
              )
          }
        )
      }
