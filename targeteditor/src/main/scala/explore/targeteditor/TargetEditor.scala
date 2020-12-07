// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.data.NonEmptyList
import cats.effect.IO
import crystal.ViewF
import crystal.react.implicits._
import explore.AppCtx
import explore.GraphQLSchemas.ObservationDB
import explore.components.graphql.LiveQueryRenderMod
import explore.implicits._
import explore.model.TargetVisualOptions
import explore.model.reusability._
import explore.target.TargetQueries._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import lucuma.core.model.Target
import lucuma.ui.reusability._
import monocle.macros.Lenses
import react.common._

final case class TargetEditor(
  id: Target.Id
) extends ReactProps[TargetEditor](TargetEditor.component)

object TargetEditor {
  type Props = TargetEditor

  @Lenses
  final case class State(options: TargetVisualOptions)

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive
  protected implicit val stateReuse: Reusability[State] = Reusability.derive

  class Backend($ : BackendScope[Props, State]) {
    def render(props: Props) =
      AppCtx.withCtx { implicit appCtx =>
        LiveQueryRenderMod[ObservationDB,
                           TargetEditQuery.Data,
                           Option[TargetEditQuery.Data.Target]
        ](
          TargetEditQuery.query(props.id),
          _.target,
          NonEmptyList.of(TargetEditSubscription.subscribe[IO](props.id))
        ) { targetOpt =>
          <.div(
            targetOpt.get.whenDefined { _ =>
              val stateView = ViewF.fromState[IO]($).zoom(State.options)
              TargetBody(props.id, targetOpt.zoom(_.get)(f => _.map(f)), stateView)
            }
          )
        }
      }
  }

  val component =
    ScalaComponent
      .builder[Props]
      .initialState(State(TargetVisualOptions.Default))
      .renderBackend[Backend]
      .configure(Reusability.shouldComponentUpdate)
      .build

}
