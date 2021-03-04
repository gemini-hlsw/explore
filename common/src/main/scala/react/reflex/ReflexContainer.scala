// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.reflex

import japgolly.scalajs.react._
import japgolly.scalajs.react.raw.JsNumber
import react.common._

import scala.scalajs.js.annotation.JSImport

import scalajs.js

object ReflexContainer {

  @js.native
  @JSImport("react-reflex", "ReflexContainer")
  private object RawComponent extends js.Object

  @js.native
  trait Props extends js.Object {
    var orientation: js.UndefOr[String]
    var maxRecDepth: js.UndefOr[JsNumber]
    var windowResizeAware: js.UndefOr[Boolean]
    var className: js.UndefOr[String]
    var style: js.UndefOr[js.Object]
  }

  object Props {
    def apply(
      orientation:       js.UndefOr[Orientation] = js.undefined,
      maxRecDepth:       js.UndefOr[JsNumber] = js.undefined,
      windowResizeAware: js.UndefOr[Boolean] = js.undefined,
      clazz:             js.UndefOr[Css] = js.undefined,
      style:             js.UndefOr[js.Object] = js.undefined // TODO Use GenericComponentPA mechanism
    ): Props = {
      val p = (new js.Object).asInstanceOf[Props]
      orientation.foreach(v => p.orientation = v.toJs)
      maxRecDepth.foreach(v => p.maxRecDepth = v)
      windowResizeAware.foreach(v => p.windowResizeAware = v)
      clazz.foreach(v => p.className = v.htmlClass)
      style.foreach(v => p.style = v)
      p
    }
  }

  private val component = JsComponent[Props, Children.Varargs, Null](RawComponent)

  def apply(
    orientation:       js.UndefOr[Orientation] = js.undefined,
    maxRecDepth:       js.UndefOr[JsNumber] = js.undefined,
    windowResizeAware: js.UndefOr[Boolean] = js.undefined,
    clazz:             js.UndefOr[Css] = js.undefined,
    style:             js.UndefOr[js.Object] = js.undefined // TODO Use GenericComponentPA mechanism
  )(children:          CtorType.ChildArg*) = component(
    Props(orientation, maxRecDepth, windowResizeAware, clazz, style)
  )(children: _*)
}
