// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.toastify

import japgolly.scalajs.react._
import react.common._

import scalajs.js
import scalajs.js.annotation.JSImport

case class ToastContainer(
  position: js.UndefOr[String] = js.undefined
) extends GenericComponentP[ToastContainer.Props] {
  override protected def cprops    = ToastContainer.props(this)
  override protected val component = ToastContainer.component
}

object ToastContainer {
  @js.native
  @JSImport("react-toastify", "ToastContainer")
  private object RawComponent extends js.Object

  @js.native
  trait Props extends js.Object {
    var position: js.UndefOr[String]
  }

  protected def props(p: ToastContainer): Props =
    rawprops(p.position)

  protected def rawprops(
    position: js.UndefOr[String] = js.undefined
  ): Props = {
    val p = (new js.Object).asInstanceOf[Props]
    position.foreach(v => p.position = v)
    p
  }

  private val component = JsComponent[Props, Children.None, Null](RawComponent)
}
