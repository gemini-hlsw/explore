// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.Monad
import cats.Monoid
import cats.effect.IO
import cats.syntax.all._
import crystal.ViewF
import crystal.ViewOptF
import crystal.react.implicits._
import crystal.react.reuse._
import explore.Icons
import explore.common.TargetEnvQueriesGQL
import explore.components.Tile
import explore.components.ui.ExploreStyles
import explore.implicits._
import explore.model.TargetEnvGroup
import explore.model.TargetIdSet
import explore.model.TargetVisualOptions
import explore.model.reusability._
import explore.optics._
import explore.schemas.implicits._
import explore.targets.TargetSelectionPopup
import explore.undo.UndoStacks
import japgolly.scalajs.react._
import japgolly.scalajs.react.callback.CallbackCats._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.SiderealTarget
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.reusability._
import monocle.function.Index._
import react.common.ReactFnProps
import react.semanticui.elements.button._
import react.semanticui.shorthand._
import react.semanticui.sizes._

final case class TargetEnvEditor(
  userId:           User.Id,
  targetEnv:        View[TargetEnvGroup],
  undoStacks:       View[Map[TargetIdSet, UndoStacks[IO, SiderealTarget]]],
  searching:        View[Set[TargetIdSet]],
  options:          View[TargetVisualOptions],
  hiddenColumns:    View[Set[String]],
  renderInTitle:    Tile.RenderInTitle
)(implicit val ctx: AppContextIO)
    extends ReactFnProps[TargetEnvEditor](TargetEnvEditor.component)

object TargetEnvEditor {
  type Props = TargetEnvEditor

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive

  // This can go into crystal.
  implicit class ViewFOptOps[F[_], A](val view: ViewF[F, Option[A]]) extends AnyVal {
    def mapValue[B](f: ViewF[F, A] => B): Option[B] =
      view.get.map(a => f(view.zoom(_ => a)(f => _.map(f))))
  }

  implicit class ViewOptFOps[F[_], A](val view: ViewOptF[F, A]) extends AnyVal {
    def mapValue[B](f: ViewF[F, A] => B)(implicit F: Monad[F], ev: Monoid[F[Unit]]): Option[B] =
      view.get.map(a => f(ViewF[F, A](a, (mod, cb) => view.modCB(mod, _.foldMap(cb)))))
  }

  // This can go into crystal.
  implicit class ViewFOps[F[_], A](val view: ViewF[F, A]) extends AnyVal {
    def unsafeNarrow[B <: A]: ViewF[F, B] =
      view.zoom(_.asInstanceOf[B])(modB => a => modB(a.asInstanceOf[B]))
  }

  // private def newTarget(name: NonEmptyString): SiderealTarget =
  //   SiderealTarget(name, SiderealTracking.const(Coordinates.Zero), SortedMap.empty)

  private def insertSiderealTarget(
    targetEnv:      View[TargetEnvGroup],
    target:         SiderealTarget,
    selectedTarget: View[Option[TargetIdSet]]
  )(implicit ctx:   AppContextIO): IO[Unit] =
    TargetEnvQueriesGQL.AddSiderealTarget
      .execute(
        targetEnv.get.id.targetEnvIds.toList,
        target.toCreateInput
      ) >>= { response =>
      val targetIds = response.updateScienceTargetList.flatMap(_.edits.map(_.target.id))

      TargetIdSet
        .fromTargetIdList(targetIds)
        .map(id => selectedTarget.set(id.some).to[IO])
        .orEmpty
    }

  protected val component =
    ScalaFnComponent
      .withHooks[Props]
      // selectedTargetIdState
      .useStateBy(_.targetEnv.get.scienceTargets.headOption.map(_._1))
      // adding
      .useState(false)
      // reset "loading" for add button when science targets change, which indicates server roundtrip is over
      .useEffectWithDepsBy((props, _, _) => props.targetEnv.get.scienceTargets)((_, _, adding) =>
        _ => adding.setState(false)
      )
      .renderWithReuse { (props, selectedTargetIdState, adding) =>
        implicit val ctx = props.ctx

        // TODO We will add this generic state => view conversion in crystal
        val selectedTargetId =
          View[Option[TargetIdSet]](
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
                .always(_ match {
                  case t @ SiderealTarget(_, _, _) =>
                    insertSiderealTarget(props.targetEnv, t, selectedTargetId).runAsync
                  case _                           => Callback.empty
                })
            )
            // InputModal(
            //   "Create new Target",
            //   initialValue = None,
            //   label = "Name",
            //   placeholder = "Target name",
            //   okLabel = "Create",
            //   onComplete = Reuse.by(props.targetEnv.get.id)((name: NonEmptyString) =>
            //     adding.setState(true) >>
            //       insertSiderealTarget(props.targetEnv,
            //                            name,
            //                            props.searching,
            //                            selectedTargetId
            //       ).runAsyncAndForget
            //   ),
            //   trigger = Reuse.by(adding.value)(
            //     Button(
            //       size = Tiny,
            //       compact = true,
            //       clazz = ExploreStyles.VeryCompact,
            //       disabled = adding.value,
            //       icon = Icons.New,
            //       loading = adding.value,
            //       content = "Add",
            //       labelPosition = LabelPosition.Left
            //     ): VdomNode
            //   )
            // )
          ),
          TargetTable(
            props.targetEnv.zoom(TargetEnvGroup.scienceTargets),
            props.hiddenColumns,
            selectedTargetId,
            props.renderInTitle
          ),
          selectedTargetId.get
            .flatMap[VdomElement] { targetId =>
              val selectedTargetView =
                props.targetEnv
                  .zoom(TargetEnvGroup.scienceTargets)
                  .zoom(index(targetId)(indexTreeSeqMap[TargetIdSet, Target]))

              selectedTargetView.mapValue(targetView =>
                targetView.get match {
                  case SiderealTarget(_, _, _) =>
                    SiderealTargetEditor(
                      props.userId,
                      targetId,
                      targetView
                        .unsafeNarrow[SiderealTarget],
                      props.undoStacks.zoom(atMapWithDefault(targetId, UndoStacks.empty)),
                      props.searching,
                      props.options
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
