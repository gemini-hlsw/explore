// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.resizeDetector

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.Builder
import japgolly.scalajs.react.vdom.html_<^._
import japgolly.scalajs.react.{ raw => Raw }
import org.scalajs.dom.html
import react.common._

import scala.scalajs.js.annotation.JSImport

import scalajs.js
import scalajs.js.|

object ResizeDetector {

  implicit class BuilderOps(b: Builder) {
    def addRefFn[A](refFn: Raw.React.RefFn[A]): Unit =
      b.addAttr("ref", refFn)
  }

  @js.native
  @JSImport("react-resize-detector", JSImport.Default)
  private object RawComponent extends js.Object

  @js.native
  trait DimensionsJS extends js.Object {
    val height: js.UndefOr[Int]
    val width: js.UndefOr[Int]
  }

  @js.native
  trait RenderPropsJS extends DimensionsJS {
    val targetRef: Raw.React.RefFn[html.Element]
  }

  trait Dimensions  {
    val height: Option[Int]
    val width: Option[Int]
  }
  object Dimensions {
    implicit val dimensionsReuse: Reusability[Dimensions] = Reusability.by(d => (d.height, d.width))
  }

  case class RenderProps(height: Option[Int], width: Option[Int], targetRef: TagMod)
      extends Dimensions
  object RenderProps {
    def apply(renderPropsJS: RenderPropsJS): RenderProps =
      RenderProps(renderPropsJS.height.toOption,
                  renderPropsJS.width.toOption,
                  TagMod.fn(_.addRefFn(renderPropsJS.targetRef))
      )
  }

  protected type RenderJS = js.Function1[RenderPropsJS, Raw.React.Node | Null]

  type Render = RenderProps => VdomNode

  sealed trait RefreshMode extends Product with Serializable

  object RefreshMode {
    implicit val enumValue: EnumValue[RefreshMode] = EnumValue.toLowerCaseString

    case object Throttle extends RefreshMode
    case object Debounce extends RefreshMode
  }

  sealed trait ObserveBox extends Product with Serializable

  object ObserveBox {
    implicit val enumValue: EnumValue[ObserveBox] = EnumValue.instance(_ match {
      case Content            => "content-box"
      case Border             => "border-box"
      case DevicePixelContent => "device-pixel-content-box"
    })

    case object Content            extends ObserveBox
    case object Border             extends ObserveBox
    case object DevicePixelContent extends ObserveBox
  }

  @js.native
  trait RefreshOptions extends js.Object {
    var leading: js.UndefOr[Boolean]
    var trailing: js.UndefOr[Boolean]
  }
  object RefreshOptions {
    def apply(
      leading:  js.UndefOr[Boolean] = js.undefined,
      trailing: js.UndefOr[Boolean] = js.undefined
    ): RefreshOptions = {
      val p = (new js.Object).asInstanceOf[RefreshOptions]
      leading.foreach(v => p.leading = v)
      trailing.foreach(v => p.trailing = v)
      p
    }
  }

  @js.native
  trait ObserverOptions extends js.Object {
    var box: js.UndefOr[String]
  }
  object ObserverOptions {
    def apply(
      box: js.UndefOr[ObserveBox] = js.undefined
    ): ObserverOptions = {
      val p = (new js.Object).asInstanceOf[ObserverOptions]
      box.toJs.foreach(v => p.box = v)
      p
    }
  }

  @js.native
  trait Props extends js.Object {
    var children: RenderJS
    var onResize: js.UndefOr[js.Function2[Int, Int, Unit]]
    var handleHeight: js.UndefOr[Boolean]
    var handleWidth: js.UndefOr[Boolean]
    var skipOnMount: js.UndefOr[Boolean]
    var refreshMode: js.UndefOr[String]
    var refreshRate: js.UndefOr[Int]
    var refreshOptions: js.UndefOr[RefreshOptions]
    var observerOptions: js.UndefOr[ObserverOptions]
  }

  object Props {
    def apply(
      children:        Render,
      onResize:        js.UndefOr[(Int, Int) => Unit] = js.undefined,
      handleHeight:    js.UndefOr[Boolean] = js.undefined,
      handleWidth:     js.UndefOr[Boolean] = js.undefined,
      skipOnMount:     js.UndefOr[Boolean] = js.undefined,
      refreshMode:     js.UndefOr[RefreshMode] = js.undefined,
      refreshRate:     js.UndefOr[Int] = js.undefined,
      refreshOptions:  js.UndefOr[RefreshOptions] = js.undefined,
      observerOptions: js.UndefOr[ObserverOptions] = js.undefined
    ): Props = {
      val p = (new js.Object).asInstanceOf[Props]
      p.children = renderPropsJS => children(RenderProps(renderPropsJS)).rawNode
      onResize.foreach(v => p.onResize = v: js.Function2[Int, Int, Unit])
      handleHeight.foreach(v => p.handleHeight = v)
      handleWidth.foreach(v => p.handleWidth = v)
      skipOnMount.foreach(v => p.skipOnMount = v)
      refreshMode.toJs.foreach(v => p.refreshMode = v)
      refreshRate.foreach(v => p.refreshRate = v)
      refreshOptions.foreach(v => p.refreshOptions = v)
      observerOptions.foreach(v => p.observerOptions = v)
      p
    }
  }

  private val component = JsComponent[Props, Children.None, Null](RawComponent)

  def apply(
    onResize:        js.UndefOr[(Int, Int) => Unit] = js.undefined,
    handleHeight:    js.UndefOr[Boolean] = js.undefined,
    handleWidth:     js.UndefOr[Boolean] = js.undefined,
    skipOnMount:     js.UndefOr[Boolean] = js.undefined,
    refreshMode:     js.UndefOr[RefreshMode] = js.undefined,
    refreshRate:     js.UndefOr[Int] = js.undefined,
    refreshOptions:  js.UndefOr[RefreshOptions] = js.undefined,
    observerOptions: js.UndefOr[ObserverOptions] = js.undefined
  )(children:        Render) = component(
    Props(children,
          onResize,
          handleHeight,
          handleWidth,
          skipOnMount,
          refreshMode,
          refreshRate,
          refreshOptions,
          observerOptions
    )
  )
}
