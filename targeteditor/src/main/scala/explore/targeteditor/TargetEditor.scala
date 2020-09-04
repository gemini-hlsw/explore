// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.effect.IO
import cats.syntax.all._
import crystal.ViewF
import crystal.react.implicits._
import explore.AppCtx
import explore.components.graphql.SubscriptionRenderMod
import explore.implicits._
import explore.model.SiderealTarget
import explore.model.TargetVisualOptions
import explore.model.reusability._
import explore.target.TargetQueries._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.function.Cons.headOption
import monocle.macros.Lenses
import react.common._

final case class TargetEditor(
  id: SiderealTarget.Id
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
        SubscriptionRenderMod[Subscription.Data, SiderealTarget](
          appCtx.clients.programs
            .subscribe(Subscription)(
              Subscription.Variables(props.id).some
            ),
          _.map(Subscription.Data.targets.composeOptional(headOption).getOption _).unNone
        ) { target =>
          val stateView = ViewF.fromState[IO]($).zoom(State.options)
          TargetBody(props.id, target, stateView)
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
