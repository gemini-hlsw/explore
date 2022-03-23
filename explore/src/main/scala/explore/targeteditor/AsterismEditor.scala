// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all._
import crystal.react.ReuseView
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.types.numeric.PosLong
import explore.Icons
import explore.common.AsterismQueries
import explore.common.TargetQueriesGQL._
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.ObsIdSet
import explore.model.TargetVisualOptions
import explore.model.TargetWithId
import explore.model.TargetWithOptId
import explore.model.reusability._
import explore.optics._
import explore.schemas.implicits._
import explore.targets.TargetSelectionPopup
import explore.undo.UndoStacks
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.reusability._
import monocle.Optional
import react.common.ReactFnProps
import react.semanticui.elements.button._
import react.semanticui.modules.checkbox._
import react.semanticui.shorthand._
import react.semanticui.sizes._

import scala.util.Random

final case class AsterismEditor(
  userId:           User.Id,
  obsIds:           ObsIdSet,
  asterism:         ReuseView[List[TargetWithId]],
  selectedTargetId: ReuseView[Option[Target.Id]],
  otherObsCount:    Target.Id ==> Int,
  undoStacks:       ReuseView[Map[Target.Id, UndoStacks[IO, Target.Sidereal]]],
  searching:        ReuseView[Set[Target.Id]],
  options:          ReuseView[TargetVisualOptions],
  hiddenColumns:    ReuseView[Set[String]],
  renderInTitle:    Tile.RenderInTitle
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[AsterismEditor](AsterismEditor.component) {}

object AsterismEditor {
  type Props = AsterismEditor

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive

  private def insertSiderealTarget(
    obsIds:         ObsIdSet,
    asterism:       ReuseView[List[TargetWithId]],
    oTargetId:      Option[Target.Id],
    target:         Target.Sidereal,
    selectedTarget: ReuseView[Option[Target.Id]]
  )(implicit ctx:   AppContextIO): Callback = {
    val (targetId, createTarget) = oTargetId.fold(
      (newId,
       (tid: Target.Id) =>
         CreateTargetMutation
           .execute("p-2", target.toCreateTargetInput(tid.some))
           .void
      )
    )(tid => (CallbackTo(tid), (_: Target.Id) => IO.unit))
    targetId.flatMap(tid =>
      asterism.mod(_ :+ TargetWithId(tid, target)) >> selectedTarget.set(tid.some) >>
        (createTarget(tid) >>
          AsterismQueries.addTargetToAsterisms[IO](
            obsIds.toList,
            tid
          )).runAsync
    )
  }

  private def onCloneTarget(
    id:       Target.Id,
    asterism: ReuseView[List[TargetWithId]],
    selected: ReuseView[Option[Target.Id]],
    newTwid:  TargetWithId
  ): Callback =
    asterism.mod(_.map(twid => if (twid.id === id) newTwid else twid)) >> selected
      .set(newTwid.id.some)

  val newId =
    CallbackTo(Random.nextInt(0xfff)).map(int => Target.Id(PosLong.unsafeFrom(int.abs.toLong + 1)))

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      // adding
      .useState(false)
      // edit target in current obs only (0), or all "instances" of the target (1)
      .useState(0)
      // reset "loading" for add button when science targets change, which indicates server roundtrip is over
      .useEffectWithDepsBy((props, _, _) => props.asterism)((_, adding, _) =>
        _ => adding.setState(false)
      )
      .useEffectWithDepsBy((props, _, _) => (props.asterism, props.selectedTargetId))(
        (props, _, _) => { case (asterism, oTargetId) =>
          // if the selected targetId is None, or not in the asterism, select the first target (if any)
          oTargetId.get
            .flatMap(id => if (asterism.get.exists(_.id === id)) id.some else none)
            .fold(props.selectedTargetId.set(asterism.get.headOption.map(_.id)))(_ =>
              Callback.empty
            )
        }
      )
      .renderWithReuse { (props, adding, editScope) =>
        implicit val ctx = props.ctx

        val selectedTargetId = props.selectedTargetId

        React.Fragment(
          props.renderInTitle(
            TargetSelectionPopup(
              trigger = Reuse.by(adding.value)(
                Button(
                  size = Tiny,
                  compact = true,
                  clazz = ExploreStyles.VeryCompact,
                  disabled = adding.value,
                  icon = Icons.New,
                  loading = adding.value,
                  content = "Add",
                  labelPosition = LabelPosition.Left
                )
              ),
              onSelected = Reuse
                .by((props.obsIds, props.asterism, selectedTargetId))(_ match {
                  case TargetWithOptId(oid, t @ Target.Sidereal(_, _, _, _)) =>
                    insertSiderealTarget(props.obsIds, props.asterism, oid, t, selectedTargetId)
                  case _                                                     => Callback.empty
                })
            )
          ),
          TargetTable(
            props.obsIds,
            props.asterism,
            props.hiddenColumns,
            selectedTargetId,
            props.renderInTitle
          ),
          selectedTargetId.get
            .flatMap[VdomElement] { targetId =>
              val optional =
                Optional[List[TargetWithId], Target](_.find(_.id === targetId).map(_.target))(
                  target =>
                    _.map { twid =>
                      if (twid.id === targetId) TargetWithId(targetId, target) else twid
                    }
                )

              val selectedTargetView = props.asterism.zoom(optional)

              val otherObsCount = props.otherObsCount(targetId)
              val plural        = if (otherObsCount === 1) "" else "s"

              selectedTargetView.mapValue(targetView =>
                targetView.get match {
                  case t @ Target.Sidereal(_, _, _, _) =>
                    <.div(
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
                        props.undoStacks.zoom(atMapWithDefault(targetId, UndoStacks.empty)),
                        props.searching,
                        props.options,
                        onClone = Reuse
                          .currying(targetId, props.asterism, props.selectedTargetId)
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
