// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.conditions

import scala.scalajs.js

import cats.implicits._
import crystal.implicits._
import explore.AppMain
import explore.implicits._
import explore.components.graphql.SubscriptionRenderMod
import explore.conditions.ConditionsQueries._
import explore.model.Conditions
import explore.model.RootModel
import explore.model.reusability._
import gem.Observation
import gem.ProgramId
import gsp.math.Index
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import monocle.function.Cons.headOption

import js.annotation._
import explore.AppCtx
import japgolly.scalajs.react.Reusable

@JSExportTopLevel("Test")
object Test extends AppMain {

  private val obsId = Observation
    .Id(ProgramId.Science.fromString.getOption("GS-2020A-DS-1").get, Index.One)

  val reusableRenderer: Reusable[View[Conditions] => VdomNode] =
    Reusable.fn { conditions =>
      ConditionsPanel(obsId, conditions)
    }

  override def rootComponent(view: View[RootModel]): VdomElement =
    AppCtx.withCtx { implicit appCtx =>
      SubscriptionRenderMod[Subscription.Data, Conditions](
        appCtx.clients.programs
          .subscribe(Subscription)(
            Subscription.Variables(obsId.format).some
          ),
        _.map(
          Subscription.Data.conditions.composeOptional(headOption).getOption _
        ).unNone
      )(reusableRenderer)
    }

}
