// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.moon

import japgolly.scalajs.react._
import react.common.GenericFnComponentP
import react.common.RenderFn

import scala.annotation.nowarn

import scalajs.js
import js.annotation.JSImport

final case class MoonPhase(
  phase:      js.UndefOr[Double] = js.undefined,
  size:       js.UndefOr[Int] = js.undefined,
  lightColor: js.UndefOr[String] = js.undefined,
  darkColor:  js.UndefOr[String] = js.undefined,
  border:     js.UndefOr[String] = js.undefined,
  rotation:   js.UndefOr[String] = js.undefined
) extends GenericFnComponentP[MoonPhase.MoonPhaseProps] {
  override protected def cprops                          = MoonPhase.props(this)
  @inline def render: RenderFn[MoonPhase.MoonPhaseProps] = MoonPhase.component(cprops)
}

object MoonPhase {

  @js.native
  @JSImport("react-moon", JSImport.Default)
  object RawComponent extends js.Function1[js.Any, js.Any] {
    def apply(i: js.Any): js.Any = js.native
  }

  @nowarn
  @js.native
  trait MoonPhaseProps extends js.Object {
    var phase: js.UndefOr[Double]      = js.native
    var size: js.UndefOr[Int]          = js.native
    var lightColor: js.UndefOr[String] = js.native
    var darkColor: js.UndefOr[String]  = js.native
    var border: js.UndefOr[String]     = js.native
    var rotation: js.UndefOr[String]   = js.native
  }

  def props(q: MoonPhase): MoonPhaseProps = {
    val p = (new js.Object).asInstanceOf[MoonPhaseProps]
    q.phase.foreach(v => p.phase = v)
    q.size.foreach(v => p.size = v)
    q.lightColor.foreach(v => p.lightColor = v)
    q.darkColor.foreach(v => p.darkColor = v)
    q.border.foreach(v => p.border = v)
    q.rotation.foreach(v => p.rotation = v)
    p
  }

  private val component =
    JsFnComponent[MoonPhaseProps, Children.None](RawComponent)
}
