// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react

import scalajs.js
import scalajs.js.|
import scalajs.js.annotation.JSImport
import japgolly.scalajs.react.vdom.all._
import japgolly.scalajs.react.callback.Callback
import japgolly.scalajs.react.facade.React
import japgolly.scalajs.react.facade.ReactDOM.DomNode
import scala.annotation.nowarn

package toastify {
  @js.native
  @nowarn
  //  https://fkhadra.github.io/react-toastify/api/toast
  trait ToastOptions extends js.Object {
    var toastId: String             = js.native
    var autoClose: Boolean | Double = js.native
    var closeButton: Boolean        = js.native
    var closeOnClick: Boolean       = js.native
    var onClose: js.Function0[Unit] = js.native
  }

  object ToastOptions {
    def apply(
      toastId:     js.UndefOr[String] = js.undefined,
      autoClose:   js.UndefOr[Boolean | Double] = js.undefined,
      closeButton: js.UndefOr[Boolean] = js.undefined,
      onClose:     js.UndefOr[Callback] = js.undefined
    ): ToastOptions = {
      val p = (new js.Object).asInstanceOf[ToastOptions]
      toastId.foreach(q => p.toastId = q)
      autoClose.foreach(q => p.autoClose = q)
      closeButton.foreach(q => p.closeButton = q)
      onClose.foreach(q => p.onClose = () => q.runNow())
      p
    }
  }

  @js.native
  @JSImport("react-toastify", "toast")
  @nowarn
  object toast extends js.Object {
    def apply(text: String): Unit = js.native

    def info(node: React.Element, options: ToastOptions = ToastOptions()): Unit = js.native
    // def info(node: String, options: ToastOptions = ToastOptions()): Unit = js.native
  }
}
