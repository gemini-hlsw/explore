// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.reflex

import explore.syntax.ui.*
import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.TagMod
import react.common.Css
import react.common.GenericComponentPAC

import scala.scalajs.js.annotation.JSImport

import scalajs.js

final case class ReflexElement(
  propagateDimensions:     js.UndefOr[Boolean] = js.undefined,
  propagateDimensionsRate: js.UndefOr[Double] = js.undefined,
  resizeHeight:            js.UndefOr[Boolean] = js.undefined,
  resizeWidth:             js.UndefOr[Boolean] = js.undefined,
  size:                    js.UndefOr[Double] = js.undefined,
  minSize:                 js.UndefOr[Double] = js.undefined,
  maxSize:                 js.UndefOr[Double] = js.undefined,
  flex:                    js.UndefOr[Double] = js.undefined,
  direction:               js.UndefOr[Direction] = js.undefined,
  onStartResize:           js.UndefOr[ResizeEvent => Callback] = js.undefined,
  onStopResize:            js.UndefOr[ResizeEvent => Callback] = js.undefined,
  onResize:                js.UndefOr[ResizeEvent => Callback] = js.undefined,
  clazz:                   js.UndefOr[Css] = js.undefined,
  withHandle:              js.UndefOr[Boolean] = js.undefined,
  override val modifiers:  Seq[TagMod] = Seq.empty
) extends GenericComponentPAC[ReflexElement.Props, ReflexElement] {
  override protected def cprops                     = ReflexElement.props(this)
  override protected val component                  = ReflexElement.component
  override def addModifiers(modifiers: Seq[TagMod]) = copy(modifiers = this.modifiers ++ modifiers)
}

object ReflexElement {

  @js.native
  @JSImport("react-reflex", "ReflexElement")
  private object RawComponent extends js.Object

  @js.native
  trait Props extends js.Object {
    var propagateDimensions: js.UndefOr[Boolean]
    var propagateDimensionsRate: js.UndefOr[Double]
    var resizeHeight: js.UndefOr[Boolean]
    var resizeWidth: js.UndefOr[Boolean]
    var size: js.UndefOr[Double]
    var minSize: js.UndefOr[Double]
    var maxSize: js.UndefOr[Double]
    var flex: js.UndefOr[Double]
    var direction: js.UndefOr[Direction.JsType]
    var onStartResize: js.UndefOr[js.Function1[ResizeEvent, Unit]]
    var onStopResize: js.UndefOr[js.Function1[ResizeEvent, Unit]]
    var onResize: js.UndefOr[js.Function1[ResizeEvent, Unit]]
    var className: js.UndefOr[String]
    var style: js.UndefOr[js.Object]
    var withHandle: js.UndefOr[Boolean]
  }

  protected def props(p: ReflexElement): Props =
    rawprops(
      p.propagateDimensions,
      p.propagateDimensionsRate,
      p.resizeHeight,
      p.resizeWidth,
      p.size,
      p.minSize,
      p.maxSize,
      p.flex,
      p.direction,
      p.onStartResize,
      p.onStopResize,
      p.onResize,
      p.clazz,
      p.withHandle
    )

  protected def rawprops(
    propagateDimensions:     js.UndefOr[Boolean] = js.undefined,
    propagateDimensionsRate: js.UndefOr[Double] = js.undefined,
    resizeHeight:            js.UndefOr[Boolean] = js.undefined,
    resizeWidth:             js.UndefOr[Boolean] = js.undefined,
    size:                    js.UndefOr[Double] = js.undefined,
    minSize:                 js.UndefOr[Double] = js.undefined,
    maxSize:                 js.UndefOr[Double] = js.undefined,
    flex:                    js.UndefOr[Double] = js.undefined,
    direction:               js.UndefOr[Direction] = js.undefined,
    onStartResize:           js.UndefOr[ResizeEvent => Callback] = js.undefined,
    onStopResize:            js.UndefOr[ResizeEvent => Callback] = js.undefined,
    onResize:                js.UndefOr[ResizeEvent => Callback] = js.undefined,
    clazz:                   js.UndefOr[Css] = js.undefined,
    withHandle:              js.UndefOr[Boolean] = js.undefined
  ): Props = {
    val p = (new js.Object).asInstanceOf[Props]
    propagateDimensions.foreach(v => p.propagateDimensions = v)
    propagateDimensionsRate.foreach(v => p.propagateDimensionsRate = v)
    resizeHeight.foreach(v => p.resizeHeight = v)
    resizeWidth.foreach(v => p.resizeWidth = v)
    size.foreach(v => p.size = v)
    minSize.foreach(v => p.minSize = v)
    maxSize.foreach(v => p.maxSize = v)
    flex.foreach(v => p.flex = v)
    direction.foreach(v => p.direction = v.toJs)
    onStartResize.toJs.foreach(v => p.onStartResize = v)
    onStopResize.toJs.foreach(v => p.onStopResize = v)
    onResize.toJs.foreach(v => p.onResize = v)
    clazz.foreach(v => p.className = v.htmlClass)
    withHandle.foreach(v => p.withHandle = v)
    p
  }

  private val component = JsComponent[Props, Children.Varargs, Null](RawComponent)

}
