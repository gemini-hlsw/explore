// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react

import japgolly.scalajs.react.*
import japgolly.scalajs.react.callback.Callback
import japgolly.scalajs.react.facade.React
import japgolly.scalajs.react.vdom.all.*
import lucuma.ui.syntax.all.*
import react.common.Css
import react.common.GenericComponentP

import scala.annotation.nowarn

import scalajs.js
import scalajs.js.|
import scalajs.js.annotation.JSImport

package toastify {
  case class ToastContainer(
    position: js.UndefOr[Position] = js.undefined,
    theme:    js.UndefOr[Theme] = js.undefined,
    clazz:    js.UndefOr[Css] = js.undefined
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
      var theme: js.UndefOr[String]
      var className: js.UndefOr[String]
    }

    protected def props(p: ToastContainer): Props =
      rawprops(p.position, p.theme, p.clazz)

    protected def rawprops(
      position: js.UndefOr[Position] = js.undefined,
      theme:    js.UndefOr[Theme] = js.undefined,
      clazz:    js.UndefOr[Css] = js.undefined
    ): Props = {
      val p = (new js.Object).asInstanceOf[Props]
      position.foreach(v => p.position = v.undefToJs)
      theme.foreach(v => p.theme = v.undefToJs)
      clazz.foreach(v => p.className = v.htmlClass)
      p
    }

    private val component = JsComponent[Props, Children.None, Null](RawComponent)
  }
  import react.common.EnumValue

  sealed trait Position extends Product with Serializable
  object Position {
    implicit val enumValue: EnumValue[Position] = EnumValue.instance[Position] {

      case TopRight     => "top-right"
      case TopCenter    => "top-center"
      case TopLeft      => "top-left"
      case BottomRight  => "bottom-right"
      case BottomCenter => "bottom-center"
      case BottomLeft   => "bottom-left"
    }

    case object TopRight     extends Position
    case object TopCenter    extends Position
    case object TopLeft      extends Position
    case object BottomRight  extends Position
    case object BottomCenter extends Position
    case object BottomLeft   extends Position
  }

  sealed trait Theme extends Product with Serializable
  object Theme {
    implicit val enumValue: EnumValue[Theme] = EnumValue.toLowerCaseString[Theme]

    case object Light   extends Theme
    case object Dark    extends Theme
    case object Colored extends Theme
  }

  @js.native
  @nowarn
  //  https://fkhadra.github.io/react-toastify/api/toast
  trait ToastOptions extends js.Object {
    var toastId: String                         = js.native
    var position: js.UndefOr[String]            = js.native
    var onClose: js.Function0[Unit]             = js.native
    var autoClose: js.UndefOr[Boolean | Double] = js.native
    var closeButton: Boolean                    = js.native
    var closeOnClick: Boolean                   = js.native
    var theme: js.UndefOr[String]               = js.native
  }

  object ToastOptions {
    def apply(
      toastId:      js.UndefOr[String] = js.undefined,
      position:     js.UndefOr[Position] = js.undefined,
      onClose:      js.UndefOr[Callback] = js.undefined,
      autoClose:    js.UndefOr[Boolean | Double] = js.undefined,
      closeButton:  js.UndefOr[Boolean] = js.undefined,
      closeOnClick: js.UndefOr[Boolean] = js.undefined,
      theme:        js.UndefOr[Theme] = js.undefined
    ): ToastOptions = {
      val p = (new js.Object).asInstanceOf[ToastOptions]
      toastId.foreach(q => p.toastId = q)
      position.foreach(v => p.position = v.undefToJs)
      onClose.foreach(q => p.onClose = () => q.runNow())
      autoClose.foreach((q: Boolean | Double) => p.autoClose = q)
      closeButton.foreach(q => p.closeButton = q)
      closeOnClick.foreach(q => p.closeOnClick = q)
      theme.foreach(v => p.theme = v.undefToJs)
      p
    }
  }

  @js.native
  @JSImport("react-toastify", "toast")
  @nowarn
  object rawToast extends js.Object {
    def apply(text: String): Unit = js.native

    def info(node: React.Element, options: ToastOptions = ToastOptions()): Unit  = js.native
    def apply(node: React.Element, options: ToastOptions = ToastOptions()): Unit = js.native
    def dismiss(id: String): Unit                                                = js.native
  }

  object toast {
    def apply(node: VdomTag, options: ToastOptions = ToastOptions()): Unit =
      rawToast(node.rawNode.asInstanceOf[React.Element], options)

    def dismiss(id: String): Unit =
      rawToast.dismiss(id)

    def dismissCB(id: String): Callback =
      Callback(rawToast.dismiss(id))
  }
}
