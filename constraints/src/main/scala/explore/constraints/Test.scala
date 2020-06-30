// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.constraints

import java.util.UUID

import scala.scalajs.js

import cats.implicits._
import crystal.implicits._
import explore.AppCtx
import explore.AppMain
import explore.components.graphql.SubscriptionRenderMod
import explore.constraints.ConstraintsQueries._
import explore.implicits._
import explore.model.Constraints
import explore.model.Page
import explore.model.RootModel
import explore.model.reusability._
import gem.Observation
import gem.ProgramId
import gsp.math.Index
import japgolly.scalajs.react.extra.router.RouterLogic
import japgolly.scalajs.react.vdom.VdomElement
import japgolly.scalajs.react.vdom.html_<^._
import monocle.function.Cons.headOption

import js.annotation._

@JSExportTopLevel("ConsTest")
object Test extends AppMain {

  private val constraintsId = UUID.fromString("608c8407-63a5-4d26-970c-587486af57da")

  override protected def rootComponent(view: View[RootModel]): VdomElement =
    constraintsSubscription(constraintsId) { constraints =>
      ConstraintsPanel(constraintsId, constraints)
    }

}
