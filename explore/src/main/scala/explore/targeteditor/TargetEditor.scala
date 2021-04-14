// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.data.NonEmptyList
import cats.effect.IO
import crystal.ViewF
import crystal.react.implicits._
import explore.AppCtx
import explore.common.TargetQueriesGQL._
import explore.common.UserPreferencesQueries._
import explore.common.UserPreferencesQueriesGQL._
import explore.components.graphql.LiveQueryRenderMod
import explore.implicits._
import explore.model.Constants
import explore.model.TargetVisualOptions
import explore.model.reusability._
import explore.schemas.ObservationDB
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.ui.reusability._
import monocle.macros.Lenses
import react.common._

final case class TargetEditor(
  uid:              User.Id,
  tid:              Target.Id,
  searching:        View[Set[Target.Id]]
)(implicit val ctx: AppContextIO)
    extends ReactProps[TargetEditor](TargetEditor.component)

object TargetEditor {
  type Props = TargetEditor

  @Lenses
  final case class State(options: TargetVisualOptions)

  object State {
    val fovAngle = options.composeLens(TargetVisualOptions.fovAngle)
  }

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive
  protected implicit val stateReuse: Reusability[State] = Reusability.derive

  class Backend($ : BackendScope[Props, State]) {
    def render(props: Props) =
      AppCtx.using { implicit appCtx =>
        LiveQueryRenderMod[ObservationDB,
                           TargetEditQuery.Data,
                           Option[TargetEditQuery.Data.Target]
        ](
          TargetEditQuery.query(props.tid),
          _.target,
          NonEmptyList.of(TargetEditSubscription.subscribe[IO](props.tid))
        ) { targetOpt =>
          targetOpt.get.map { _ =>
            val stateView = ViewF.fromState[IO]($).zoom(State.options)
            TargetBody(props.uid,
                       props.tid,
                       targetOpt.zoom(_.get)(f => _.map(f)),
                       props.searching,
                       stateView
            )
          }
        }
      }
  }

  val component =
    ScalaComponent
      .builder[Props]
      .initialState(State(TargetVisualOptions.Default))
      .renderBackend[Backend]
      .componentDidMount { $ =>
        val p            = $.props
        implicit val ctx = p.ctx
        UserTargetPreferencesQuery
          .queryWithDefault[IO](p.uid, p.tid, Constants.InitialFov)
          .flatMap(v => $.modStateIn[IO](State.fovAngle.set(v)))
          .runAsyncAndForgetCB
      }
      .configure(Reusability.shouldComponentUpdate)
      .build

}
