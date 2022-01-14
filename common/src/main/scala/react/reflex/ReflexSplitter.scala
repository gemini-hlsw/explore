// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.reflex

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.TagMod
import react.common._

import scala.scalajs.js.annotation.JSImport

import scalajs.js

final case class ReflexSplitter(
  propagate:              js.UndefOr[Boolean] = js.undefined,
  onStartResize:          js.UndefOr[ResizeEvent => Callback] = js.undefined,
  onStopResize:           js.UndefOr[ResizeEvent => Callback] = js.undefined,
  onResize:               js.UndefOr[ResizeEvent => Callback] = js.undefined,
  clazz:                  js.UndefOr[Css] = js.undefined,
  override val modifiers: Seq[TagMod] = Seq.empty
) extends GenericComponentPA[ReflexSplitter.Props, ReflexSplitter] {
  override protected def cprops                     = ReflexSplitter.props(this)
  override protected val component                  = ReflexSplitter.component
  override def addModifiers(modifiers: Seq[TagMod]) = copy(modifiers = this.modifiers ++ modifiers)
}

object ReflexSplitter {

  @js.native
  @JSImport("react-reflex", "ReflexSplitter")
  private object RawComponent extends js.Object

  @js.native
  trait Props extends js.Object {
    var propagate: js.UndefOr[Boolean]
    var onStartResize: js.UndefOr[js.Function1[ResizeEvent, Unit]]
    var onStopResize: js.UndefOr[js.Function1[ResizeEvent, Unit]]
    var onResize: js.UndefOr[js.Function1[ResizeEvent, Unit]]
    var className: js.UndefOr[String]
    var style: js.UndefOr[js.Object]
  }

  protected def props(p: ReflexSplitter): Props =
    rawprops(p.propagate, p.onStartResize, p.onStopResize, p.onResize, p.clazz)

  protected def rawprops(
    propagate:     js.UndefOr[Boolean] = js.undefined,
    onStartResize: js.UndefOr[ResizeEvent => Callback] = js.undefined,
    onStopResize:  js.UndefOr[ResizeEvent => Callback] = js.undefined,
    onResize:      js.UndefOr[ResizeEvent => Callback] = js.undefined,
    clazz:         js.UndefOr[Css] = js.undefined
  ): Props = {
    val p = (new js.Object).asInstanceOf[Props]
    propagate.foreach(v => p.propagate = v)
    onStartResize.toJs.foreach(v => p.onStartResize = v)
    onStopResize.toJs.foreach(v => p.onStopResize = v)
    onResize.toJs.foreach(v => p.onResize = v)
    clazz.foreach(v => p.className = v.htmlClass)
    p
  }

  private val component = JsComponent[Props, Children.None, Null](RawComponent)
}
