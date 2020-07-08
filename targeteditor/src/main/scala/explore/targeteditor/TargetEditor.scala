// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.implicits._
import explore.AppCtx
import explore.components.graphql.SubscriptionRenderMod
import explore.implicits._
import explore.model.SiderealTarget
import explore.model.reusability._
import explore.target.TargetQueries._
import japgolly.scalajs.react._
import japgolly.scalajs.react.MonocleReact._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.function.Cons.headOption
import react.common._
import explore.model.TargetVisualOptions
import monocle.macros.Lenses

final case class TargetEditor(
  id: SiderealTarget.Id
) extends ReactProps[TargetEditor](TargetEditor.component)

object TargetEditor {
  type Props = TargetEditor

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive

  @Lenses
  final case class State(options: TargetVisualOptions)

  class Backend($ : BackendScope[Props, State]) {
    def render(props: Props, state: State) =
      AppCtx.withCtx { implicit appCtx =>
        // implicit val cs = appCtx.cs
        SubscriptionRenderMod[Subscription.Data, SiderealTarget](
          appCtx.clients.programs
            .subscribe(Subscription)(
              Subscription.Variables(props.id).some
            ),
          _.map(Subscription.Data.targets.composeOptional(headOption).getOption _).unNone
        ) { target =>
          TargetBody(props.id,
                     target,
                     props.constraints,
                     state.options,
                     $.setStateL(State.options)(_)
          )
        }
      }
  }

  val component =
    ScalaComponent
      .builder[Props]
      .initialState(State(TargetVisualOptions.Default))
      .renderBackend[Backend]
      .build

}
