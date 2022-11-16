// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all.*
import clue.TransactionalClient
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
import explore.model.ObsIdSet
import explore.model.ScienceMode
import explore.model.SiderealTargetWithId
import explore.model.TargetWithId
import explore.model.TargetWithOptId
import explore.model.reusability.*
import explore.model.reusability.given
import explore.optics.*
import explore.optics.all.*
import explore.targets.TargetSelectionPopup
import explore.undo.UndoStacks
import japgolly.scalajs.react.*
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.util.DefaultEffects.{Sync => DefaultS}
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.core.data.Zipper
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.PosAngleConstraint
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.util.NewType
import lucuma.schemas.ObservationDB
import lucuma.ui.primereact.*
import lucuma.ui.reusability.*
import lucuma.ui.syntax.all.*
import lucuma.ui.syntax.all.given
import monocle.Lens
import monocle.std.option.some
import org.typelevel.log4cats.Logger
import queries.common.TargetQueriesGQL.*
import queries.schemas.odb.ODBConversions.*
import queries.schemas.odb.ObsQueries
import react.common.ReactFnProps
import react.primereact.Button
import react.semanticui.modules.checkbox.*

import java.time.Instant

case class AsterismEditor(
  userId:        User.Id,
  programId:     Program.Id,
  obsIds:        ObsIdSet,
  asterism:      View[Option[Asterism]],
  potVizTime:    Pot[View[Option[Instant]]],
  scienceMode:   Option[ScienceMode],
  posAngle:      Option[PosAngleConstraint],
  constraints:   Option[ConstraintSet],
  wavelength:    Option[Wavelength],
  currentTarget: Option[Target.Id],
  setTarget:     (Option[Target.Id], SetRouteVia) => Callback,
  otherObsCount: Target.Id => Int,
  undoStacks:    View[Map[Target.Id, UndoStacks[IO, Target.Sidereal]]],
  searching:     View[Set[Target.Id]],
  renderInTitle: Tile.RenderInTitle
) extends ReactFnProps(AsterismEditor.component)

object AsterismEditor {
  private type Props = AsterismEditor

  private object EditScope extends NewType[Boolean]:
    inline def AllInstances: EditScope = EditScope(true)
    inline def CurrentOnly: EditScope  = EditScope(false)

  private type EditScope = EditScope.Type

  private object AreAdding extends NewType[Boolean]
  private type AreAdding = AreAdding.Type

  private def insertSiderealTarget(
    programId:      Program.Id,
    obsIds:         ObsIdSet,
    asterism:       View[Option[Asterism]],
    oTargetId:      Option[Target.Id],
    target:         Target.Sidereal,
    selectedTarget: View[Option[Target.Id]],
    adding:         View[AreAdding]
  )(using TransactionalClient[IO, ObservationDB], Logger[IO]): IO[Unit] =
    val targetId: IO[Target.Id] = oTargetId.fold(
      CreateTargetMutation
        .execute(target.toCreateTargetInput(programId))
        .map(_.createTarget.target.id)
    )(IO(_))

    adding.async.set(AreAdding(true)) >>
      targetId
        .flatMap { tid =>
          val newTarget   = TargetWithId(tid, target)
          val asterismAdd = asterism.async.mod {
            case a @ Some(_) => a.map(_.add(newTarget))
            case _           => Asterism.one(newTarget).some
          }
          asterismAdd >>
            selectedTarget.async.set(tid.some) >>
            AsterismQueries.addTargetToAsterisms[IO](programId, obsIds.toList, tid)
        }
        .guarantee(adding.async.set(AreAdding(false)))

  private def onCloneTarget(
    id:        Target.Id,
    asterism:  View[Option[Asterism]],
    setTarget: (Option[Target.Id], SetRouteVia) => Callback
  )(
    newTwid:   TargetWithId
  ): Callback =
    asterism
      .zoom(Asterism.fromTargetsList.reverse.asLens)
      .mod(_.map(twid => if (twid.id === id) newTwid else twid)) >>
      setTarget(newTwid.id.some, SetRouteVia.HistoryPush)

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useContext(AppContext.ctx)
      .useStateView(AreAdding(false))
      .useState(EditScope.CurrentOnly)
      .useEffectWithDepsBy((props, _, _, _) => (props.asterism.get, props.currentTarget)) {
        (props, _, _, _) => (asterism, oTargetId) =>
          // if the selected targetId is None, or not in the asterism, select the first target (if any)
          // Need to replace history here.
          oTargetId match {
            case None                                                     =>
              asterism.foldMap(a => props.setTarget(a.focus.id.some, SetRouteVia.HistoryReplace))
            case Some(current) if asterism.exists(_.focus.id === current) => Callback.empty
            case current @ Some(_)                                        =>
              val inAsterism = current.exists(id => props.asterism.get.exists(_.hasId(id)))
              val focus      = props.asterism.get.map(_.focus.id)
              if (!inAsterism)
                props.setTarget(focus, SetRouteVia.HistoryReplace)
              else props.setTarget(current, SetRouteVia.HistoryReplace)
          }
      }
      // full screen aladin
      .useStateView(AladinFullScreen.Normal)
      .render { (props, ctx, adding, editScope, fullScreen) =>
        import ctx.given

        val targetView: View[Option[Target.Id]] =
          View[Option[Target.Id]](
            props.currentTarget,
            { (f, cb) =>
              val newValue = f(props.currentTarget)
              props.setTarget(newValue, SetRouteVia.HistoryPush) >> cb(newValue)
            }
          )

        // Save the time here. this works for the obs and target tabs
        val vizTimeView = props.potVizTime.map(_.withOnMod { t =>
          ObsQueries.updateVisualizationTime[IO](props.programId, props.obsIds.toList, t).runAsync
        })

        val vizTime = props.potVizTime.toOption.flatMap(_.get)

        <.div(
          ExploreStyles.AladinFullScreen.when(fullScreen.get.value),
          props.renderInTitle(
            TargetSelectionPopup(
              props.programId,
              trigger = Button(
                severity = Button.Severity.Success,
                disabled = adding.get.value,
                icon = Icons.New,
                loading = adding.get.value,
                label = "Add"
              ).tiny.compact,
              onSelected = _ match {
                case TargetWithOptId(oid, t @ Target.Sidereal(_, _, _, _)) =>
                  insertSiderealTarget(
                    props.programId,
                    props.obsIds,
                    props.asterism,
                    oid,
                    t,
                    targetView,
                    adding
                  ).runAsync

                case _ =>
                  Callback.empty
              }
            )
          ),
          props.renderInTitle(VizTimeEditor(vizTimeView)),
          TargetTable(
            props.userId.some,
            props.programId,
            props.obsIds,
            props.asterism,
            targetView,
            vizTime,
            props.renderInTitle,
            fullScreen.get
          ),
          props.currentTarget
            .flatMap[VdomElement] { targetId =>
              val targetInAsterism   = Asterism.targetOptional(targetId)
              val selectedTargetView = props.asterism.zoom(targetInAsterism)

              val otherObsCount = props.otherObsCount(targetId)
              val plural        = if (otherObsCount === 1) "" else "s"

              selectedTargetView.mapValue(targetView =>
                targetView.get match {
                  case TargetWithId(_, t @ Target.Sidereal(_, _, _, _)) =>
                    <.div(
                      ExploreStyles.TargetTileEditor,
                      <.div(
                        ExploreStyles.SharedEditWarning,
                        s"${t.name.value} is in ${otherObsCount} other observation$plural. Edits here should apply to:",
                        Checkbox(
                          name = "editScope",
                          label =
                            if (props.obsIds.size === 1) "only this observation"
                            else "only the current observations",
                          value = 0,
                          checked = editScope.value === EditScope.CurrentOnly,
                          onChange = (_: Boolean) => editScope.setState(EditScope.CurrentOnly)
                        ),
                        Checkbox(
                          name = "editScope",
                          label = "all observations of this target",
                          value = 1,
                          checked = editScope.value === EditScope.AllInstances,
                          onChange = (_: Boolean) => editScope.setState(EditScope.AllInstances)
                        )
                      ).when(otherObsCount > 0),
                      props.asterism.mapValue(asterism =>
                        SiderealTargetEditor(
                          props.userId,
                          asterism,
                          vizTime,
                          props.posAngle,
                          props.scienceMode,
                          props.constraints,
                          props.wavelength,
                          props.undoStacks.zoom(atMapWithDefault(targetId, UndoStacks.empty)),
                          props.searching,
                          onClone = onCloneTarget(targetId, props.asterism, props.setTarget) _,
                          obsIdSubset =
                            if (otherObsCount > 0 && editScope.value === EditScope.CurrentOnly)
                              props.obsIds.some
                            else none,
                          fullScreen = fullScreen
                        )
                      )
                    )
                  case _                                                =>
                    <.div("Non-sidereal targets not supported")
                }
              )
            }
        )
      }
}
