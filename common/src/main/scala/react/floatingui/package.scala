// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.floatingui

import cats.syntax.all.*
import japgolly.scalajs.react._
import japgolly.scalajs.react.facade.React
import japgolly.scalajs.react.vdom.TopNode
import org.scalajs.dom
import org.scalajs.dom.html
import react.common.EnumValue
import react.common.syntax.callback.*
import react.common.syntax.enumValue.*

import scala.scalajs.js
import scala.scalajs.js.JSConverters._
import scala.scalajs.js.annotation.JSImport
import scala.scalajs.js.|

enum Side:
  case Top, Right, Bottom, Left

enum Alignment:
  case Start, End

enum Strategy:
  case Absolute, Fixed

enum AlignedPlacement:
  case TopStart, TopEnd, RightStart, RightEnd, BottomStart, BottomEnd, LeftStart, LeftEnd

enum Placement: // Side | AlignedPlacement
  case Top, Right, Bottom, Left, TopStart, TopEnd, RightStart, RightEnd, BottomStart, BottomEnd,
    LeftStart, LeftEnd

object Placement:
  def fromString(s: String): Option[Placement] = s match {
    case "top"          => Placement.Top.some
    case "right"        => Placement.Right.some
    case "bottom"       => Placement.Bottom.some
    case "left"         => Placement.Left.some
    case "top-start"    => Placement.TopStart.some
    case "top-end"      => Placement.TopEnd.some
    case "right-start"  => Placement.RightStart.some
    case "right-end"    => Placement.RightEnd.some
    case "botton-start" => Placement.BottomStart.some
    case "bottom-end"   => Placement.BottomEnd.some
    case "left-start"   => Placement.LeftStart.some
    case "left-end"     => Placement.LeftEnd.some
    case _              => none
  }

given EnumValue[Strategy]  = EnumValue.toLowerCaseString[Strategy]
given EnumValue[Placement] = EnumValue.instance {
  case Placement.Top         => "top"
  case Placement.Right       => "right"
  case Placement.Bottom      => "bottom"
  case Placement.Left        => "left"
  case Placement.TopStart    => "top-start"
  case Placement.TopEnd      => "top-end"
  case Placement.RightStart  => "right-start"
  case Placement.RightEnd    => "right-end"
  case Placement.BottomStart => "botton-start"
  case Placement.BottomEnd   => "bottom-end"
  case Placement.LeftStart   => "left-start"
  case Placement.LeftEnd     => "left-end"
}

type Middleware = js.Object

@js.native
trait UseFloatingReturn extends js.Object {

  var context: FloatingContext               = js.native
  var middlewareData: MiddlewareData         = js.native
  var placement: String                      = js.native
  val reference: facade.React.RefFn[TopNode] = js.native
  val floating: facade.React.RefFn[TopNode]  = js.native
  val strategy: String                       = js.native

  val x: js.UndefOr[Double] = js.native
  val y: js.UndefOr[Double] = js.native

}

@js.native
trait ArrowCoords extends js.Object {

  var centerOffset: Double = js.native

  var x: js.UndefOr[Double] = js.native

  var y: js.UndefOr[Double] = js.native
}

@js.native
trait MiddlewareData extends js.Object {

  var arrow: js.UndefOr[ArrowCoords] = js.native

}

@js.native
trait UseFloatingProps extends js.Object {

  var middleware: js.UndefOr[js.Array[Middleware]] = js.native

  var onOpenChange: js.UndefOr[js.Function1[Boolean, Unit]] = js.native

  var open: js.UndefOr[Boolean] = js.native

  var placement: js.UndefOr[String] = js.native

  var strategy: js.UndefOr[String] = js.native
}

object UseFloatingProps {

  inline def apply(
    placement:    js.UndefOr[Placement] = js.undefined,
    strategy:     js.UndefOr[Strategy] = js.undefined,
    open:         js.UndefOr[Boolean] = js.undefined,
    onOpenChange: js.UndefOr[Boolean => Callback] = js.undefined,
    middleware:   List[Middleware] = Nil
  ): UseFloatingProps =
    val p = js.Dynamic.literal().asInstanceOf[UseFloatingProps]
    strategy.foreach(s => p.strategy = s.toJs)
    placement.foreach(s => p.placement = s.toJs)
    open.foreach(s => p.open = s)
    onOpenChange.foreach(s => p.onOpenChange = s.toJs)
    p.middleware = middleware.toJSArray
    p

}

type Padding = Double //| SideObject

@js.native
trait ShiftOptions extends js.Object {
  var padding: js.UndefOr[Padding] = js.native
}

object ShiftOptions {

  inline def apply(
    padding: js.UndefOr[Padding] = js.undefined
  ): ShiftOptions =
    val p = js.Dynamic.literal().asInstanceOf[ShiftOptions]
    padding.foreach(s => p.padding = s)
    p
}

type OffsetValue = Double //| AlignmentAxis

type OffsetOptions = OffsetValue //| OffsetFunction

@js.native
trait FloatingContext extends UseFloatingReturn {
  var open: Boolean = js.native
}

@js.native
trait ArrowElement extends js.Object:

  var element: js.Any = js.native

  var padding: js.UndefOr[Padding] = js.native

object ArrowElement:

  inline def apply(element: facade.React.RefHandle[TopNode | Null]): ArrowElement = {
    val o = js.Dynamic.literal.asInstanceOf[ArrowElement]
    Option(element.current)
      .flatMap(_.domToHtml)
      .fold(o.element = element)(o.element = _)
    o
  }
  inline def apply(element: html.Element): ArrowElement                           = {
    val __obj = js.Dynamic.literal(element = element.asInstanceOf[js.Any])
    __obj.asInstanceOf[ArrowElement]
  }

object middleware {

  @JSImport("@floating-ui/react-dom-interactions", JSImport.Namespace)
  @js.native
  val ^ : js.Any = js.native

  /**
   * Positions an inner element of the floating element such that it is centered to the reference
   * element.
   * @see
   *   https://floating-ui.com/docs/arrow
   */
  inline def arrow(options: ArrowElement): Middleware = ^.asInstanceOf[js.Dynamic]
    .applyDynamic("arrow")(options.asInstanceOf[js.Any])
    .asInstanceOf[Middleware]

  /**
   * Changes the placement of the floating element to one that will fit if the initially specified
   * `placement` does not.
   * @see
   *   https://floating-ui.com/docs/flip
   */
  inline def flip(): Middleware                       =
    ^.asInstanceOf[js.Dynamic].applyDynamic("flip")().asInstanceOf[Middleware]
  //
  inline def offset(): Middleware                     =
    ^.asInstanceOf[js.Dynamic].applyDynamic("offset")().asInstanceOf[Middleware]
  inline def offset(value: OffsetOptions): Middleware = ^.asInstanceOf[js.Dynamic]
    .applyDynamic("offset")(value.asInstanceOf[js.Any])
    .asInstanceOf[Middleware]

  /**
   * Shifts the floating element in order to keep it in view when it will overflow a clipping
   * boundary.
   * @see
   *   https://floating-ui.com/docs/shift
   */
  inline def shift(): Middleware                      =
    ^.asInstanceOf[js.Dynamic].applyDynamic("shift")().asInstanceOf[Middleware]
  inline def shift(options: ShiftOptions): Middleware = ^.asInstanceOf[js.Dynamic]
    .applyDynamic("shift")(options.asInstanceOf[js.Any])
    .asInstanceOf[Middleware]

  inline def useHover(context: FloatingContext): ElementProps =
    ^.asInstanceOf[js.Dynamic]
      .applyDynamic("useHover")(context.asInstanceOf[js.Any])
      .asInstanceOf[ElementProps]
}

object use {
  @JSImport("@floating-ui/react-dom-interactions", JSImport.Namespace)
  @js.native
  val ^ : js.Any = js.native

  inline def useFloating(): UseFloatingReturn =
    ^.asInstanceOf[js.Dynamic].applyDynamic("useFloating")().asInstanceOf[UseFloatingReturn]
  inline def useFloating(
    props: UseFloatingProps
  ): UseFloatingReturn =
    ^.asInstanceOf[js.Dynamic].applyDynamic("useFloating")(props).asInstanceOf[UseFloatingReturn]

  inline def useInteractions(propsList: js.Array[ElementProps | Unit]): UseFloatingReturn =
    ^.asInstanceOf[js.Dynamic]
      .applyDynamic("useInteractions")(propsList.asInstanceOf[js.Any])
      .asInstanceOf[UseFloatingReturn]
}
