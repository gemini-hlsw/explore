// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all._
import crystal.react.ReuseView
import crystal.react.ReuseViewOpt
import crystal.react.View
import crystal.react.hooks._
import crystal.react.implicits._
import crystal.react.reuse._
import explore.Icons
import explore.common.AsterismQueries
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.config.ObsTimeComponent
import explore.implicits._
import explore.model.Asterism
import explore.model.ObsConfiguration
import explore.model.ObsIdSet
import explore.model.ScienceMode
import explore.model.TargetWithId
import explore.model.TargetWithOptId
import explore.model.reusability._
import explore.optics._
import explore.targets.TargetSelectionPopup
import explore.undo.UndoStacks
import japgolly.scalajs.react._
import japgolly.scalajs.react.extra.router.SetRouteVia
import japgolly.scalajs.react.util.DefaultEffects.{ Sync => DefaultS }
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.reusability._
import queries.common.TargetQueriesGQL._
import queries.schemas.implicits._
import react.common.ReactFnProps
import react.semanticui.collections.form.Form
import react.semanticui.elements.button._
import react.semanticui.modules.checkbox._
import react.semanticui.shorthand._
import react.semanticui.sizes._

final case class AsterismEditor(
  userId:           User.Id,
  programId:        Program.Id,
  obsIds:           ObsIdSet,
  asterism:         ReuseView[Option[Asterism]],
  scienceMode:      Option[ScienceMode],
  obsConf:          ReuseViewOpt[ObsConfiguration],
  currentTarget:    Option[Target.Id],
  setTarget:        (Option[Target.Id], SetRouteVia) ==> Callback,
  otherObsCount:    Target.Id ==> Int,
  undoStacks:       ReuseView[Map[Target.Id, UndoStacks[IO, Target.Sidereal]]],
  searching:        ReuseView[Set[Target.Id]],
  hiddenColumns:    ReuseView[Set[String]],
  renderInTitle:    Tile.RenderInTitle
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[AsterismEditor](AsterismEditor.component)

object AsterismEditor {
  type Props = AsterismEditor

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive

  private def insertSiderealTarget(
    programId:      Program.Id,
    obsIds:         ObsIdSet,
    asterism:       ReuseView[Option[Asterism]],
    oTargetId:      Option[Target.Id],
    target:         Target.Sidereal,
    selectedTarget: ReuseView[Option[Target.Id]],
    adding:         View[Boolean]
  )(implicit ctx:   AppContextIO): IO[Unit] = {
    val targetId: IO[Target.Id] = oTargetId.fold(
      CreateTargetMutation.execute(target.toCreateTargetInput(programId)).map(_.createTarget.id)
    )(IO(_))
    adding.async.set(true) >>
      targetId
        .flatMap { tid =>
          val newTarget   = TargetWithId(tid, target)
          val asterismAdd = asterism.async.mod {
            case a @ Some(_) => a.map(_.add(newTarget))
            case _           => Asterism.one(newTarget).some
          }
          asterismAdd >>
            selectedTarget.async.set(tid.some) >>
            AsterismQueries.addTargetToAsterisms[IO](obsIds.toList, tid)
        }
        .guarantee(adding.async.set(false))
  }

  private def onCloneTarget(
    id:        Target.Id,
    asterism:  ReuseView[Option[Asterism]],
    setTarget: (Option[Target.Id], SetRouteVia) ==> Callback,
    newTwid:   TargetWithId
  ): Callback =
    asterism
      .zoom(Asterism.fromTargetsList.reverse.asLens)
      .mod(_.map(twid => if (twid.id === id) newTwid else twid)) >>
      setTarget(newTwid.id.some, SetRouteVia.HistoryPush)

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      // adding
      .useStateViewWithReuse(false)
      // edit target in current obs only (0), or all "instances" of the target (1)
      .useState(0)
      .useEffectWithDepsBy((props, _, _) => (props.asterism, props.currentTarget, props.setTarget))(
        (_, _, _) => { case (asterism, oTargetId, setTarget) =>
          // if the selected targetId is None, or not in the asterism, select the first target (if any)
          // Need to replace history here.
          oTargetId match {
            case None                                                   =>
              asterism.get.foldMap(a => setTarget(a.baseTarget.id.some, SetRouteVia.HistoryReplace))
            case Some(current) if asterism.get.exists(_.hasId(current)) => Callback.empty
            case _                                                      =>
              setTarget(asterism.get.map(_.baseTarget.id), SetRouteVia.HistoryReplace)
          }
        }
      )
      .renderWithReuse { (props, adding, editScope) =>
        implicit val ctx = props.ctx

        val targetView: ReuseView[Option[Target.Id]] =
          Reuse.by(props.currentTarget, props.setTarget)(
            View[Option[Target.Id]](
              props.currentTarget,
              { (f, cb) =>
                val newValue = f(props.currentTarget)
                props.setTarget(newValue, SetRouteVia.HistoryPush) >> cb(newValue)
              }
            )
          )

        React.Fragment(
          props.renderInTitle(
            TargetSelectionPopup(
              props.programId,
              trigger = adding.map(a =>
                Button(
                  size = Tiny,
                  compact = true,
                  clazz = ExploreStyles.VeryCompact,
                  disabled = a.get,
                  icon = Icons.New,
                  loading = a.get,
                  content = "Add",
                  labelPosition = LabelPosition.Left
                )
              ),
              onSelected = Reuse
                .by((props.obsIds, props.asterism, targetView))(_ match {
                  case TargetWithOptId(oid, t @ Target.Sidereal(_, _, _, _)) =>
                    insertSiderealTarget(props.programId,
                                         props.obsIds,
                                         props.asterism,
                                         oid,
                                         t,
                                         targetView,
                                         adding
                    ).runAsync
                  case _                                                     => Callback.empty
                })
            )
          ),
          props.renderInTitle(
            Form(size = Small)(
              ExploreStyles.Compact,
              ExploreStyles.ObsInstantTileTitle,
              props.obsConf.zoom(ObsConfiguration.obsInstant).mapValue(ObsTimeComponent(_))
            )
          ),
          TargetTable(
            props.obsIds,
            props.asterism,
            props.hiddenColumns,
            targetView,
            props.renderInTitle
          ),
          props.currentTarget
            .flatMap[VdomElement] { targetId =>
              val selectedTargetView = props.asterism.zoom(Asterism.targetOptional(targetId))

              val otherObsCount = props.otherObsCount(targetId)
              val plural        = if (otherObsCount === 1) "" else "s"

              selectedTargetView.mapValue(targetView =>
                targetView.get match {
                  case t @ Target.Sidereal(_, _, _, _) =>
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
                          checked = editScope.value === 0,
                          onChange = (_: Boolean) => editScope.setState(0)
                        ),
                        Checkbox(name = "editScope",
                                 label = "all observations of this target",
                                 value = 1,
                                 checked = editScope.value === 1,
                                 onChange = (_: Boolean) => editScope.setState(1)
                        )
                      ).when(otherObsCount > 0),
                      SiderealTargetEditor(
                        props.userId,
                        targetId,
                        targetView.unsafeNarrow[Target.Sidereal],
                        props.obsConf.get,
                        props.scienceMode,
                        props.undoStacks.zoom(atMapWithDefault(targetId, UndoStacks.empty)),
                        props.searching,
                        onClone = Reuse
                          .currying(targetId, props.asterism, props.setTarget)
                          .in(onCloneTarget _),
                        obsIdSubset =
                          if (otherObsCount > 0 && editScope.value === 0) props.obsIds.some
                          else none
                      )
                    )
                  case _                               =>
                    <.div("Non-sidereal targets not supported")
                }
              )
            }
        )
      }
}
