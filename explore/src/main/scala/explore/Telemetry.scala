// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.syntax.show.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.vdom.html_<^.*
import lucuma.react.common.ReactFnProps
import lucuma.ui.sso.UserVault

import scala.annotation.unused
import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
trait HoneycombOptions extends js.Object {
  val apiKey: String                                     = js.native
  val serviceName: String                                = js.native
  val dataset: js.UndefOr[String]                        = js.native
  val localVisualizations: js.UndefOr[Boolean]           = js.native
  val resourceAttributes: js.UndefOr[ResourceAttributes] = js.native
  val debug: js.UndefOr[Boolean]                         = js.native
}

object HoneycombOptions {
  def apply(
    apiKey:                   String,
    serviceName:              String,
    resourceAttributes:       js.UndefOr[ResourceAttributes] = js.undefined,
    @unused instrumentations: js.UndefOr[js.Array[js.Object]] = js.undefined,
    dataset:                  js.UndefOr[String] = js.undefined,
    localVisualizations:      js.UndefOr[Boolean] = js.undefined,
    debug:                    js.UndefOr[Boolean] = js.undefined
  ): HoneycombOptions =
    js.Dynamic
      .literal(apiKey = apiKey,
               serviceName = serviceName,
               dataset = dataset,
               localVisualizations = localVisualizations,
               resourceAttributes = resourceAttributes,
               debug = debug
      )
      .asInstanceOf[HoneycombOptions]
}

@js.native
trait ResourceAttributes extends js.Object {
  val `user.id`: String   = js.native
  val `user.role`: String = js.native
}

object ResourceAttributes {
  def apply(
    userId:   String,
    userRole: String
  ): ResourceAttributes =
    js.Dynamic
      .literal(`user.id` = userId, `user.role` = userRole)
      .asInstanceOf[ResourceAttributes]

  def fromUserVault(vault: UserVault): ResourceAttributes =
    apply(vault.user.id.show, vault.user.role.name)
}

@js.native
@JSImport("@honeycombio/opentelemetry-web", "HoneycombWebSDK")
class HoneycombWebSDK(@unused val options: js.UndefOr[HoneycombOptions] = js.undefined)
    extends js.Object {
  def start(): Unit    = js.native
  def shutdown(): Unit = js.native
}

@js.native
@JSImport("@opentelemetry/auto-instrumentations-web", "getWebAutoInstrumentations")
object getWebAutoInstrumentations extends js.Object {
  def apply(
    options: js.UndefOr[js.Object] = js.undefined
  ): js.Object = js.native
}

@js.native
@JSImport("@opentelemetry/instrumentation-user-interaction", "UserInteractionInstrumentation")
class UserInteractionInstrumentation(@unused val options: js.UndefOr[js.Object] = js.undefined)
    extends js.Object

case class Observability(options: HoneycombOptions)
    extends ReactFnProps[Observability](Observability.component)

object Observability:
  private type Props = Observability

  private val component =
    ScalaFnComponent
      .withHooks[Props]
      .useEffectOnMountBy: props =>
        val sdk = new HoneycombWebSDK(props.options)
        Callback(sdk.start()) *> CallbackTo(Callback(sdk.shutdown()))
      .render(_ => EmptyVdom)
