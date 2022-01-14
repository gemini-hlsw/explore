// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.reflex

import japgolly.scalajs.react._
import japgolly.scalajs.react.vdom.TagMod
import react.common._

import scala.scalajs.js.annotation.JSImport

import scalajs.js

// ReflexHandle must either:
// 1) Be the first direct child of a ReactElement; or otherwise:
// 2) Be wrapped within a ReflexWithHandle, and the provided parameter be passed on opaquely.
final case class ReflexHandle(
  propagate:              js.UndefOr[Boolean] = js.undefined,
  onStartResize:          js.UndefOr[ResizeEvent => Callback] = js.undefined,
  onStopResize:           js.UndefOr[ResizeEvent => Callback] = js.undefined,
  onResize:               js.UndefOr[ResizeEvent => Callback] = js.undefined,
  provided:               js.UndefOr[HandleProps] = js.undefined,
  clazz:                  js.UndefOr[Css] = js.undefined,
  override val modifiers: Seq[TagMod] = Seq.empty
) extends GenericComponentPAC[ReflexHandle.Props, ReflexHandle] {
  override protected def cprops                     = ReflexHandle.props(this)
  override protected val component                  = ReflexHandle.component
  override def addModifiers(modifiers: Seq[TagMod]) = copy(modifiers = this.modifiers ++ modifiers)
}

object ReflexHandle {

  @js.native
  @JSImport("react-reflex", "ReflexHandle")
  private object RawComponent extends js.Object

  @js.native
  trait Props extends ReflexSplitter.Props with HandleProps

  protected def props(p: ReflexHandle): Props =
    rawprops(p.propagate, p.onStartResize, p.onStopResize, p.onResize, p.provided, p.clazz)

  protected def rawprops(
    propagate:     js.UndefOr[Boolean] = js.undefined,
    onStartResize: js.UndefOr[ResizeEvent => Callback] = js.undefined,
    onStopResize:  js.UndefOr[ResizeEvent => Callback] = js.undefined,
    onResize:      js.UndefOr[ResizeEvent => Callback] = js.undefined,
    provided:      js.UndefOr[HandleProps] = js.undefined,
    clazz:         js.UndefOr[Css] = js.undefined
  ): Props = {
    val p = (new js.Object).asInstanceOf[Props]
    propagate.foreach(v => p.propagate = v)
    onStartResize.toJs.foreach(v => p.onStartResize = v)
    onStopResize.toJs.foreach(v => p.onStopResize = v)
    onResize.toJs.foreach(v => p.onResize = v)
    provided.foreach { v =>
      import cats.syntax.all._
      (v.index.toOption, v.events.toOption).tupled match {
        case None                  =>
          throw new Exception(
            "ReflexHandle received provided property without injected properties. Did you specify \"withHandle = true\" on enclosing ReflexElement?"
          )
        case Some((index, events)) =>
          p.index = index
          p.events = events
      }
    }
    clazz.foreach(v => p.className = v.htmlClass)
    p
  }

  private val component = JsComponent[Props, Children.Varargs, Null](RawComponent)
}
