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
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.*

@js.native
trait HoneycombOptions extends js.Object {
  val apiKey: String                                     = js.native
  val serviceName: String                                = js.native
  val serviceVersion: js.UndefOr[String]                 = js.native
  val dataset: js.UndefOr[String]                        = js.native
  val localVisualizations: js.UndefOr[Boolean]           = js.native
  val resourceAttributes: js.UndefOr[ResourceAttributes] = js.native
  val debug: js.UndefOr[Boolean]                         = js.native
  val instrumentations: js.UndefOr[js.Array[js.Object]]  = js.native
  val endpoint: js.UndefOr[String]                       = js.native
  val spanProcessors: js.UndefOr[js.Array[js.Object]]    = js.native
  val skipOptionsValidation: js.UndefOr[Boolean]         = js.native
  val contextManager: js.UndefOr[js.Object]              = js.native
}

object HoneycombOptions {
  def apply(
    apiKey:                String,
    serviceName:           String,
    serviceVersion:        js.UndefOr[String] = js.undefined,
    resourceAttributes:    js.UndefOr[ResourceAttributes] = js.undefined,
    dataset:               js.UndefOr[String] = js.undefined,
    localVisualizations:   js.UndefOr[Boolean] = js.undefined,
    debug:                 js.UndefOr[Boolean] = js.undefined,
    instrumentations:      js.UndefOr[js.Array[js.Object]] = js.undefined,
    endpoint:              js.UndefOr[String] = js.undefined,
    spanProcessors:        js.UndefOr[js.Array[js.Object]] = js.undefined,
    skipOptionsValidation: js.UndefOr[Boolean] = js.undefined,
    contextManager:        js.UndefOr[js.Object] = js.undefined
  ): HoneycombOptions =
    js.Dynamic
      .literal(
        apiKey = apiKey,
        serviceName = serviceName,
        serviceVersion = serviceVersion,
        dataset = dataset,
        localVisualizations = localVisualizations,
        resourceAttributes = resourceAttributes,
        debug = debug,
        instrumentations = instrumentations,
        endpoint = endpoint,
        spanProcessors = spanProcessors,
        skipOptionsValidation = skipOptionsValidation,
        contextManager = contextManager
      )
      .asInstanceOf[HoneycombOptions]
}

@js.native
trait ResourceAttributes extends js.Object

object ResourceAttributes {
  def apply(
    userId:   String,
    userRole: List[String],
    fullName: String
  ): ResourceAttributes =
    // https://opentelemetry.io/docs/specs/semconv/registry/attributes/user/
    js.Dynamic
      .literal(`user.id` = userId, `user.roles` = userRole.toJSArray, `user.fullName` = fullName)
      .asInstanceOf[ResourceAttributes]

  def fromUserVault(vault: UserVault): ResourceAttributes =
    apply(vault.user.id.show, vault.roleNames, vault.user.displayName)
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
