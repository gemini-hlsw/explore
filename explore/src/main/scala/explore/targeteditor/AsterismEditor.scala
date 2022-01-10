// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all._
import crystal.react.View
import crystal.react.implicits._
import crystal.react.reuse._
import eu.timepit.refined.types.numeric.PosLong
import explore.Icons
import explore.common.AsterismQueries
import explore.common.TargetQueries
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.ObsIdSet
import explore.model.TargetVisualOptions
import explore.model.TargetWithId
import explore.model.reusability._
import explore.optics._
import explore.targets.TargetSelectionPopup
import explore.undo.UndoStacks
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.SiderealTarget
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.reusability._
import monocle.Optional
import react.common.ReactFnProps
import react.semanticui.elements.button._
import react.semanticui.shorthand._
import react.semanticui.sizes._

import scala.util.Random

final case class AsterismEditor(
  userId:           User.Id,
  obsIds:           ObsIdSet,
  asterism:         View[List[TargetWithId]],
  undoStacks:       View[Map[Target.Id, UndoStacks[IO, SiderealTarget]]],
  searching:        View[Set[Target.Id]],
  options:          View[TargetVisualOptions],
  hiddenColumns:    View[Set[String]],
  renderInTitle:    Tile.RenderInTitle
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[AsterismEditor](AsterismEditor.component)

object AsterismEditor {
  type Props = AsterismEditor

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive

  private def insertSiderealTarget(
    obsIds:         ObsIdSet,
    asterism:       View[List[TargetWithId]],
    oTargetId:      Option[Target.Id],
    target:         SiderealTarget,
    selectedTarget: View[Option[Target.Id]]
  )(implicit ctx:   AppContextIO): Callback = {
    val (targetId, createTarget) = oTargetId.fold(
      (newId, ((tid: Target.Id) => TargetQueries.createSiderealTarget[IO](tid, target)))
    )(tid => (CallbackTo(tid), (_: Target.Id) => IO.unit))
    targetId.flatMap(tid =>
      asterism.mod(_ :+ (tid, target)) >> selectedTarget.set(tid.some) >>
        (createTarget(tid) >>
          AsterismQueries.addTargetToAsterisms[IO](
            obsIds.toList,
            tid
          )).runAsync
    )
  }

  val newId =
    CallbackTo(Random.nextInt(0xfff)).map(int => Target.Id(PosLong.unsafeFrom(int.abs.toLong + 1)))

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      // selectedTargetIdState
      .useStateBy(_.asterism.get.headOption.map(_._1))
      // adding
      .useState(false)
      // reset "loading" for add button when science targets change, which indicates server roundtrip is over
      .useEffectWithDepsBy((props, _, _) => props.asterism.get)((_, _, adding) =>
        _ => adding.setState(false)
      )
      .renderWithReuse { (props, selectedTargetIdState, adding) =>
        implicit val ctx = props.ctx

        // TODO We will add this generic state => view conversion in crystal
        val selectedTargetId =
          View[Option[Target.Id]](
            selectedTargetIdState.value,
            (mod, _) => selectedTargetIdState.modState(mod)
          )

        <.div(
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
                  case (oid, t @ SiderealTarget(_, _, _, _)) =>
                    insertSiderealTarget(props.obsIds, props.asterism, oid, t, selectedTargetId)
                  case _                                     => Callback.empty
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
                Optional[List[TargetWithId], Target](_.find(_._1 === targetId).map(_._2))(target =>
                  _.map(twid => if (twid._1 === targetId) (targetId, target) else twid)
                )

              val selectedTargetView = props.asterism.zoom(optional)

              selectedTargetView.mapValue(targetView =>
                targetView.get match {
                  case SiderealTarget(_, _, _, _) =>
                    SiderealTargetEditor(
                      props.userId,
                      targetId,
                      targetView
                        .unsafeNarrow[SiderealTarget],
                      props.undoStacks.zoom(atMapWithDefault(targetId, UndoStacks.empty)),
                      props.searching,
                      props.options
                    )
                  case _                          =>
                    <.div("Non-sidereal targets not supported")
                }
              )
            }
            .whenDefined
        )
      }
}
