package reactmoon

import japgolly.scalajs.react._
import react.common._
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
  override protected def cprops                        = MoonPhase.props(this)
  @inline def render: Render[MoonPhase.MoonPhaseProps] = MoonPhase.component(cprops)
}

object MoonPhase {

  @js.native
  @JSImport("react-moon", JSImport.Default)
  object RawComponent extends js.Function1[js.Any, js.Any] {
    def apply(i: js.Any): js.Any = js.native
  }

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
    JsComponent[MoonPhaseProps, Children.None, Null](RawComponent)
}
