// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.targeteditor

import cats.implicits._
import crystal.react.implicits._
import explore.AppCtx
import explore.components.graphql.SubscriptionRenderMod
import explore.implicits._
import explore.model.Constraints
import explore.model.SiderealTarget
import explore.model.reusability._
import explore.target.TargetQueries._
import gem.Observation
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.function.Cons.headOption
import react.common._

final case class TargetEditor(
  id:          SiderealTarget.Id,
  constraints: Option[Constraints] = None
) extends ReactProps[TargetEditor](TargetEditor.component)

object TargetEditor {
  type Props = TargetEditor

  protected implicit val propsReuse: Reusability[Props] = Reusability.derive

  class Backend() {
    def render(props: Props) =
      AppCtx.withCtx { implicit appCtx =>
        SubscriptionRenderMod[Subscription.Data, SiderealTarget](
          appCtx.clients.programs
            .subscribe(Subscription)(
              Subscription.Variables(props.id).some
            ),
          _.map(Subscription.Data.targets.composeOptional(headOption).getOption _).unNone
        ) { target =>
          TargetBody(props.id, target, props.constraints)
        }
      }
  }

  val component =
    ScalaComponent
      .builder[Props]
      .backend(_ => new Backend())
      .renderBackend
      .build

}
