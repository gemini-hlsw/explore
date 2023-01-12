// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.shortcuts

import eu.timepit.refined.types.string.NonEmptyString
import japgolly.scalajs.react.Callback
import lucuma.core.util.NewType
import lucuma.refined.*
import react.hotkeys.HotkeysCallback
import react.hotkeys.HotkeysEvent

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
        .getOrElse(Callback.empty)

val GoToObs         = Shortcut("o")
val GoToTargets     = Shortcut("t")
val GoToProposals   = Shortcut("r")
val GoToConstraints = Shortcut("n")
val GoToOverview    = Shortcut("w")
val GoToSummary     = Shortcut("s")

val Esc           = Shortcut("ESC")
val ShortcutsHelp = Shortcut("F1")

val Down = Shortcut("j")
val Up   = Shortcut("k")

val CopyAlt1 = Shortcut("y")
val CopyAlt2 = Shortcut("ctrl+c")
val CopyAlt3 = Shortcut("meta+c")

val CopyKeys = List(CopyAlt1, CopyAlt2, CopyAlt3)

val PasteAlt1 = Shortcut("p")
val PasteAlt2 = Shortcut("ctrl+v")
val PasteAlt3 = Shortcut("meta+v")

val PasteKeys = List(PasteAlt1, PasteAlt2, PasteAlt3)
