// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import crystal.ViewF
import crystal.react.implicits._
import explore.components.Tile
import explore.implicits._
import explore.model.ScienceTarget
import explore.model.SiderealScienceTarget
import explore.model.TargetEnv
import explore.model.TargetVisualOptions
import explore.model.reusability._
import explore.optics._
import explore.undo.UndoStacks
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.SiderealTarget
import lucuma.core.model.User
import lucuma.ui.reusability._
import monocle.function.At._
import react.common.ReactFnProps

final case class TargetEnvEditor(
  userId:        User.Id,
  targetEnv:     View[TargetEnv],
  undoStacks:    View[Map[ScienceTarget.Id, UndoStacks[IO, SiderealTarget]]],
  searching:     View[Set[ScienceTarget.Id]],
  options:       View[TargetVisualOptions],
  hiddenColumns: View[Set[String]],
  renderInTitle: Tile.RenderInTitle
) extends ReactFnProps[TargetEnvEditor](TargetEnvEditor.component)

object TargetEnvEditor {
  type Props = TargetEnvEditor

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive

  // This can go into crystal.
  implicit class ViewFOptOps[F[_], A](val view: ViewF[F, Option[A]]) extends AnyVal {
    def mapValue[B](f: ViewF[F, A] => B): Option[B] =
      view.get.map(a => f(view.zoom(_ => a)(f => _.map(f))))
  }

  // This can go into crystal.
  implicit class ViewFOps[F[_], A](val view: ViewF[F, A]) extends AnyVal {
    def unsafeNarrow[B <: A]: ViewF[F, B] =
      view.zoom(_.asInstanceOf[B])(modB => a => modB(a.asInstanceOf[B]))
  }

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      .useStateBy(_.targetEnv.get.scienceTargets.headOption.map(_._1))
      .renderWithReuse { (props, selectedTargetId) =>
        <.div(
          TargetTable(
            props.targetEnv.get.scienceTargets.toList.map(_._2),
            props.hiddenColumns,
            props.renderInTitle
            // onSelect
          ),
          selectedTargetId.value
            .flatMap[VdomElement] { targetId =>
              val selectedTargetView =
                props.targetEnv
                  .zoom(TargetEnv.scienceTargets)
                  .zoom(at(targetId)(atTreeSeqMap[ScienceTarget.Id, ScienceTarget]))

              selectedTargetView.mapValue(targetView =>
                targetView.get match {
                  case SiderealScienceTarget(_, _) =>
                    SiderealTargetEditor(
                      props.userId,
                      targetId,
                      targetView
                        .unsafeNarrow[SiderealScienceTarget]
                        .zoom(SiderealScienceTarget.target),
                      props.undoStacks.zoom(atMapWithDefault(targetId, UndoStacks.empty)),
                      props.searching,
                      props.options,
                      props.renderInTitle
                    )
                  case _                           =>
                    <.div("Non-sidereal targets not supported")
                }
              )
            }
            .whenDefined
        )
      }
}
