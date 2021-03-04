// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.reflex

import japgolly.scalajs.react._
import react.common._

import scala.scalajs.js.annotation.JSImport

import scalajs.js

object ReflexHandle {

  @js.native
  @JSImport("react-reflex", "ReflexHandle")
  private object RawComponent extends js.Object

  type Props = ReflexSplitter.Props

  object Props {
    def apply(
      propagate:     js.UndefOr[Boolean] = js.undefined,
      onStartResize: js.UndefOr[ResizeEvent => Callback] = js.undefined,
      onStopResize:  js.UndefOr[ResizeEvent => Callback] = js.undefined,
      onResize:      js.UndefOr[ResizeEvent => Callback] = js.undefined,
      clazz:         js.UndefOr[Css] = js.undefined,
      style:         js.UndefOr[js.Object] = js.undefined // TODO Use GenericComponentPA mechanism
    ): Props = {
      val p = (new js.Object).asInstanceOf[Props]
      propagate.foreach(v => p.propagate = v)
      onStartResize.toJs.foreach(v => p.onStartResize = v)
      onStopResize.toJs.foreach(v => p.onStopResize = v)
      onResize.toJs.foreach(v => p.onResize = v)
      clazz.foreach(v => p.className = v.htmlClass)
      style.foreach(v => p.style = v)
      p
    }
  }

  private val component = JsComponent[Props, Children.Varargs, Null](RawComponent)

  def apply(
    propagate:     js.UndefOr[Boolean] = js.undefined,
    onStartResize: js.UndefOr[ResizeEvent => Callback] = js.undefined,
    onStopResize:  js.UndefOr[ResizeEvent => Callback] = js.undefined,
    onResize:      js.UndefOr[ResizeEvent => Callback] = js.undefined,
    clazz:         js.UndefOr[Css] = js.undefined,
    style:         js.UndefOr[js.Object] = js.undefined // TODO Use GenericComponentPA mechanism
  )(children:      CtorType.ChildArg*) = component(
    Props(
      propagate,
      onStartResize,
      onStopResize,
      onResize,
      clazz,
      style
    )
  )(children: _*)
}
