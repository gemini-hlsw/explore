// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.target

import cats.implicits._
import explore.components.graphql.SubscriptionRenderMod
import explore.implicits._
import explore.model.SiderealTarget
import explore.target.TargetQueries._
import gem.Observation
import japgolly.scalajs.react.CatsReact._
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.function.Cons.headOption
import react.common._
import explore.AppCtx

final case class TargetEditor(
  observationId: Observation.Id,
  globalTarget:  View[Option[SiderealTarget]]
) extends ReactProps[TargetEditor](TargetEditor.component)

object TargetEditor {
  type Props = TargetEditor

  protected implicit val targetReuse: Reusability[SiderealTarget] = Reusability.byEq

  val component =
    ScalaComponent
      .builder[Props]
      .render_P { props =>
        AppCtx.withCtx { implicit appCtx =>
          SubscriptionRenderMod[Subscription.Data, SiderealTarget](
            appCtx.clients.programs
              .subscribe(Subscription)(
                Subscription.Variables(props.observationId.format).some
              ),
            _.map(Subscription.Data.targets.composeOptional(headOption).getOption _).unNone
          ) { target =>
            TargetBody(props.observationId, target, props.globalTarget)
          }
        }
      }
      .build

}
