// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.hotkeys

import japgolly.scalajs.react.*
import japgolly.scalajs.react.facade.React.HookDeps
import org.scalajs.dom

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSImport

type Ref             = Ref.Simple[dom.Element]
type HotkeysCallback = Callback | (HotkeysEvent => Callback)

@js.native
trait HotkeysOptions extends js.Object {
  var enabled: js.UndefOr[Boolean]                 = js.native
  var filterPreventDefault: js.UndefOr[Boolean]    = js.native
  var enableOnContentEditable: js.UndefOr[Boolean] = js.native
  var splitKey: js.UndefOr[String]                 = js.native
  var keyup: js.UndefOr[Boolean]                   = js.native
  var keydown: js.UndefOr[Boolean]                 = js.native
}

object HotkeysOptions {

  def apply(
    enabled:              js.UndefOr[Boolean] = js.undefined,
    filterPreventDefault: js.UndefOr[Boolean] = js.undefined,
    splitKey:             js.UndefOr[String] = js.undefined
  ): HotkeysOptions =
    val p = js.Dynamic.literal().asInstanceOf[HotkeysOptions]
    p.enabled.foreach(o => p.enabled = o)
    p.filterPreventDefault.foreach(o => p.filterPreventDefault = o)
    p.splitKey.foreach(o => p.splitKey = o)
    p
}

@js.native
trait HotkeysEvent extends js.Object {
  val alt: Boolean
  val shift: Boolean
  val meta: Boolean
  val mod: Boolean
  val keys: js.Array[String]
}

@js.native
trait UseHotkeysProps extends js.Object {
  var keys: String | js.Array[String] = js.native

  var callback: js.Function2[js.Any, HotkeysEvent, Unit] = js.native

  var options: js.UndefOr[HotkeysOptions] = js.native

}

object UseHotkeysProps {

  def apply(
    keys:     List[String],
    callback: HotkeysCallback,
    options:  js.UndefOr[HotkeysOptions] = js.undefined
  ): UseHotkeysProps =
    val p = js.Dynamic.literal().asInstanceOf[UseHotkeysProps]
    p.keys = keys.toJSArray
    p.callback = (u, h) =>
      callback match
        case c: Callback                   => c.runNow()
        case c: (HotkeysEvent => Callback) => c(h).runNow()
    options.foreach(o => p.options = o)
    p
}

@JSImport("react-hotkeys-hook", "useHotkeys")
@js.native
private val useHotkeys: js.Function4[
  String | js.Array[String],
  js.Function2[js.Any, HotkeysEvent, Unit],
  js.UndefOr[HotkeysOptions],
  js.UndefOr[HookDeps],
  facade.React.RefHandle[dom.Element | Null]
] = js.native
