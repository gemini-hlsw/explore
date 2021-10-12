package explore.targeteditor

import explore.implicits._
import lucuma.core.model.User
import explore.model.TargetEnv
import lucuma.core.model.Target
import explore.undo.UndoStacks
import explore.model.TargetVisualOptions
import cats.effect.IO
import lucuma.core.model.SiderealTarget
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import explore.components.Tile
import react.common.ReactFnProps
import lucuma.ui.reusability._
import crystal.react.implicits._
import explore.model.reusability._
import monocle.function.At._
import explore.model.ScienceTarget
import scala.collection.immutable.TreeSeqMap
import monocle.function.At
import monocle.Lens
import crystal.ViewF
import explore.optics._

final case class TargetEnvEditor(
  userId:        User.Id,
  targetEnv:     View[TargetEnv],
  undoStacks:    View[Map[Target.Id, UndoStacks[IO, SiderealTarget]]],
  searching:     View[Set[Target.Id]],
  options:       View[TargetVisualOptions],
  hiddenColumns: View[Set[String]],
  renderInTitle: Tile.RenderInTitle
) extends ReactFnProps[TargetEnvEditor](TargetEnvEditor.component)

object TargetEnvEditor {
  type Props = TargetEnvEditor

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive

  implicit def atTreeSeqMap[K, V]: At[TreeSeqMap[K, V], K, Option[V]] =
    At(i =>
      Lens((_: TreeSeqMap[K, V]).get(i))(optV => map => optV.fold(map - i)(v => map + (i -> v)))
    )

  implicit class ViewFOptOps[F[_], A](val view: ViewF[F, Option[A]]) extends AnyVal {
    def mapValue[B](f: ViewF[F, A] => B): Option[B] =
      view.get.map(a => f(view.zoom(_ => a)(f => _.map(f))))
  }

  implicit class ViewFOps[F[_], A](val view: ViewF[F, A]) extends AnyVal {
    def unsafeNarrow[B <: A]: ViewF[F, B] =
      view.zoom(_.asInstanceOf[B])(modB => a => modB(a.asInstanceOf[B]))
    // def mapValue[B](f: ViewF[F, A] => B): Option[B] =
    //   view.get.map(a => f(view.zoom(_ => a)(f => _.map(f))))
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
                  .zoom(at(targetId)(atTreeSeqMap[Target.Id, ScienceTarget[Target]]))

              selectedTargetView.mapValue(targetView =>
                targetView.get.target match {
                  case SiderealTarget(_, _, _) =>
                    SiderealTargetEditor(
                      props.userId,
                      targetId,
                      targetView
                        .unsafeNarrow[ScienceTarget[SiderealTarget]]
                        .zoom(ScienceTarget.target),
                      props.undoStacks.zoom(atMapWithDefault(targetId, UndoStacks.empty)),
                      props.searching,
                      props.options,
                      props.renderInTitle
                    )
                  case _                       =>
                    <.div("Non-sidereal targets not supported")
                }
              )
            }
            .whenDefined
        )
      }
}
