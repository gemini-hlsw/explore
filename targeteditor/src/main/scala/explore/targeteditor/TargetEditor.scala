// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.target

import cats.implicits._
import explore.components.graphql.SubscriptionRenderMod
import explore.implicits._
import explore.model.SiderealTarget
import explore.target.TargetQueries._
import gem.Observation
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.vdom.html_<^._
import monocle.function.Cons.headOption
import react.common._

final case class TargetEditor(
  observationId:    CtxIO[Observation.Id]
)(implicit val ctx: AppContextIO)
    extends ReactProps {
  @inline override def render: VdomElement = TargetEditor.component(this)
}

object TargetEditor {
  type Props = TargetEditor

  val component =
    ScalaComponent
      .builder[Props]
      .render_P { props =>
        props.observationId.withCtx { implicit appCtx =>
          SubscriptionRenderMod[Subscription.Data, SiderealTarget](
            appCtx.clients.programs
              .subscribe(Subscription)(
                Subscription.Variables(props.observationId.value.format).some
              ),
            _.map(Subscription.Data.targets.composeOptional(headOption).getOption _).unNone
          ) { view =>
            TargetBody(props.observationId.value, view)
          }
        }
      }
      .build

}
