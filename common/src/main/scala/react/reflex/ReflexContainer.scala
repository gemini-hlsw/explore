// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.reflex

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.TagMod
import react.common._

import scala.scalajs.js.annotation.JSImport

import scalajs.js

final case class ReflexContainer(
  orientation:            js.UndefOr[Orientation] = js.undefined,
  maxRecDepth:            js.UndefOr[Double] = js.undefined,
  windowResizeAware:      js.UndefOr[Boolean] = js.undefined,
  clazz:                  js.UndefOr[Css] = js.undefined,
  override val modifiers: Seq[TagMod] = Seq.empty
) extends GenericComponentPAC[ReflexContainer.Props, ReflexContainer] {
  override protected def cprops                     = ReflexContainer.props(this)
  override protected val component                  = ReflexContainer.component
  override def addModifiers(modifiers: Seq[TagMod]) = copy(modifiers = this.modifiers ++ modifiers)
}

object ReflexContainer {

  @js.native
  @JSImport("react-reflex", "ReflexContainer")
  private object RawComponent extends js.Object

  @js.native
  trait Props extends js.Object {
    var orientation: js.UndefOr[String]
    var maxRecDepth: js.UndefOr[Double]
    var windowResizeAware: js.UndefOr[Boolean]
    var className: js.UndefOr[String]
    var style: js.UndefOr[js.Object]
  }

  protected def props(p: ReflexContainer): Props =
    rawprops(p.orientation, p.maxRecDepth, p.windowResizeAware, p.clazz)

  protected def rawprops(
    orientation:       js.UndefOr[Orientation] = js.undefined,
    maxRecDepth:       js.UndefOr[Double] = js.undefined,
    windowResizeAware: js.UndefOr[Boolean] = js.undefined,
    clazz:             js.UndefOr[Css] = js.undefined
  ): Props = {
    val p = (new js.Object).asInstanceOf[Props]
    orientation.foreach(v => p.orientation = v.toJs)
    maxRecDepth.foreach(v => p.maxRecDepth = v)
    windowResizeAware.foreach(v => p.windowResizeAware = v)
    clazz.foreach(v => p.className = v.htmlClass)
    p
  }

  private val component = JsComponent[Props, Children.Varargs, Null](RawComponent)
}
