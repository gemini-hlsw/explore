// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.shortcuts

import japgolly.scalajs.react.*
import lucuma.core.util.NewType
import lucuma.react.hotkeys.HotkeysCallback
import lucuma.react.hotkeys.HotkeysEvent

import scala.scalajs.js.JSConverters.*

object Shortcut extends NewType[List[String]] {
  inline def apply(key: String): Shortcut = Shortcut(
    List(key.toLowerCase.trim.filterNot(_.isWhitespace))
  )

  // This- maybe improved to consider cases where there are more than one matching key
  def withModifiers(e: HotkeysEvent): List[Shortcut] =
    if (e.meta) e.keys.toList.map(k => Shortcut(s"meta+$k"))
    else if (e.shift) e.keys.toList.map(k => Shortcut(s"shift+$k"))
    else if (e.alt) e.keys.toList.map(k => Shortcut(s"alt+$k"))
    else if (e.mod) e.keys.toList.map(k => Shortcut(s"mod+$k"))
    else e.keys.toList.map(k => Shortcut(k))
}

type Shortcut = Shortcut.Type

extension (shortcuts: List[Shortcut]) def toHotKeys: List[String] = shortcuts.flatMap(_.value)

type ShortcutCallbacks = PartialFunction[Shortcut, Callback]

given Conversion[PartialFunction[Shortcut, Callback], HotkeysCallback] with
  def apply(p: PartialFunction[Shortcut, Callback]): HotkeysCallback =
    (e: HotkeysEvent) =>
      Shortcut
        .withModifiers(e)
        .collectFirst {
          case s if p.isDefinedAt(s) =>
            p.applyOrElse(s, _ => Callback.empty)
        }
        .headOption
        .getOrEmpty

val GoToObs         = Shortcut("o")
val GoToTargets     = Shortcut("t")
val GoToProposals   = Shortcut("r")
val GoToConstraints = Shortcut("n")
val GoToOverview    = Shortcut("w")
val GoToSummary     = Shortcut("s")

val Esc = Shortcut("ESC")

val ShortcutsHelp1 = Shortcut("F1")
val ShortcutsHelp2 = Shortcut("shift+?")

val ShortcutsHelpKeys = List(ShortcutsHelp1, ShortcutsHelp2)

val Down = Shortcut("j")
val Up   = Shortcut("k")

val CopyAlt1 = Shortcut("y")
// mod implies cmd on mac and ctrl on windows/linux
val CopyAlt2 = Shortcut("mod+c")

val CopyKeys = List(CopyAlt1, CopyAlt2)

val PasteAlt1 = Shortcut("p")
val PasteAlt2 = Shortcut("mod+v")

val PasteKeys = List(PasteAlt1, PasteAlt2)
