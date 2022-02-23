// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
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
import react.resizeDetector.hooks._
import react.semanticui.elements.button._
import react.semanticui.shorthand._
import react.semanticui.sizes._

import scala.util.Random

final case class AsterismEditor(
  userId:           User.Id,
  obsIds:           ObsIdSet,
  asterism:         View[List[TargetWithId]],
  undoStacks:       View[Map[Target.Id, UndoStacks[IO, Target.Sidereal]]],
  searching:        View[Set[Target.Id]],
  options:          View[TargetVisualOptions],
  hiddenColumns:    View[Set[String]],
  renderInTitle:    Tile.RenderInTitle
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[AsterismEditor](AsterismEditor.component) {}

object AsterismEditor {
  type Props = AsterismEditor

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive

  private def insertSiderealTarget(
    obsIds:         ObsIdSet,
    asterism:       View[List[TargetWithId]],
    oTargetId:      Option[Target.Id],
    target:         Target.Sidereal,
    selectedTarget: View[Option[Target.Id]]
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

  val newId =
    CallbackTo(Random.nextInt(0xfff)).map(int => Target.Id(PosLong.unsafeFrom(int.abs.toLong + 1)))

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      // selectedTargetIdState
      .useStateBy(_.asterism.get.headOption.map(_.id))
      // adding
      .useState(false)
      // reset "loading" for add button when science targets change, which indicates server roundtrip is over
      .useEffectWithDepsBy((props, _, _) => props.asterism.get)((_, _, adding) =>
        _ => adding.setState(false)
      )
      .useResizeDetector()
      .renderWithReuse { (props, selectedTargetIdState, adding, resize) =>
        implicit val ctx = props.ctx

        // TODO We will add this generic state => view conversion in crystal
        val selectedTargetId =
          View[Option[Target.Id]](
            selectedTargetIdState.value,
            (mod, _) => selectedTargetIdState.modState(mod)
          )

        val targetTableHeight = props.asterism.get.length match {
          case 0 => 27
          case 1 => 57
          case 2 => 91
          case _ => 110
        }
        val editorHeight      = resize.height.map(h => math.max(0, h - targetTableHeight)).getOrElse(0)

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
          <.div(^.height := s"${targetTableHeight}px",
                TargetTable(
                  props.obsIds,
                  props.asterism,
                  props.hiddenColumns,
                  selectedTargetId,
                  props.renderInTitle
                )
          ),
          <.div(
            ^.height     := s"${editorHeight}px",
            selectedTargetId.get
              .flatMap[VdomElement] { targetId =>
                val optional =
                  Optional[List[TargetWithId], Target](_.find(_.id === targetId).map(_.target))(
                    target =>
                      _.map(twid =>
                        if (twid.id === targetId) TargetWithId(targetId, target) else twid
                      )
                  )

                val selectedTargetView = props.asterism.zoom(optional)

                selectedTargetView.mapValue(targetView =>
                  targetView.get match {
                    case Target.Sidereal(_, _, _, _) =>
                      SiderealTargetEditor(
                        props.userId,
                        targetId,
                        targetView
                          .unsafeNarrow[Target.Sidereal],
                        props.undoStacks.zoom(atMapWithDefault(targetId, UndoStacks.empty)),
                        props.searching,
                        props.options
                      )
                    case _                           =>
                      <.div("Non-sidereal targets not supported")
                  }
                )
              }
          )
        )
      }
}
