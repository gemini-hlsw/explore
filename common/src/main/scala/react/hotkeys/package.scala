// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package react.hotkeys

// import cats.syntax.all.*
import japgolly.scalajs.react.*
import japgolly.scalajs.react.facade.React.HookDeps
import japgolly.scalajs.react.vdom.TopNode
import org.scalajs.dom
// import org.scalajs.dom.html

import scala.scalajs.js
import scala.scalajs.js.JSConverters.*
import scala.scalajs.js.annotation.JSImport
// import scala.scalajs.js.|

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

  // var deps: js.UndefOr[js.Array[js.Any]] = js.native
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
