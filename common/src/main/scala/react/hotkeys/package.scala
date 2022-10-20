// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.hotkeys

import japgolly.scalajs.react.*
import japgolly.scalajs.react.facade.React.HookDeps
import japgolly.scalajs.react.vdom.TopNode
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
    enabled: js.UndefOr[Boolean] = js.undefined
  ): HotkeysOptions =
    val p = js.Dynamic.literal().asInstanceOf[HotkeysOptions]
    p.enabled.foreach(o => p.enabled = o)
    p
}

@js.native
trait HotkeysEvent extends js.Object {
  var key: String
}

@js.native
trait UseHotkeysProps extends js.Object {
  var keys: String = js.native

  var callback: js.Function2[js.Any, HotkeysEvent, Unit] = js.native

  var options: js.UndefOr[HotkeysOptions] = js.native

}

object UseHotkeysProps {

  def apply(
    keys:     String,
    callback: HotkeysCallback,
    options:  js.UndefOr[HotkeysOptions] = js.undefined
  ): UseHotkeysProps =
    val p = js.Dynamic.literal().asInstanceOf[UseHotkeysProps]
    p.keys = keys
    p.callback = (_, h) =>
      callback match
        case c: Callback                   => c.runNow()
        case c: (HotkeysEvent => Callback) => c(h).runNow()
    options.foreach(o => p.options = o)
    p
}

@JSImport("react-hotkeys-hook", "useHotkeys")
@js.native
private val useHotkeys: js.Function4[
  String,
  js.Function2[js.Any, HotkeysEvent, Unit],
  js.UndefOr[HotkeysOptions],
  js.UndefOr[HookDeps],
  facade.React.RefHandle[dom.Element | Null]
] = js.native

@JSImport("react-hotkeys-hook", "isHotkeyPressed")
@js.native
val isHotkeyPressed: js.Function1[
  String | Double,
  Boolean
] = js.native

// Don't make these val or you will get always false
def isCmdCtrlPressed: Boolean = isHotkeyPressed("cmd") || isHotkeyPressed("ctrl")

def isShiftPressed: Boolean = isHotkeyPressed("shift")
