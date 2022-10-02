// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.shortcuts

import eu.timepit.refined.types.string.NonEmptyString
import lucuma.refined.*
import japgolly.scalajs.react.Callback
import react.hotkeys.HotkeysEvent
import react.hotkeys.HotkeysCallback
import lucuma.core.util.NewType
import scala.scalajs.js.|

object Shortcut extends NewType[String]
type Shortcut = Shortcut.Type

extension (shortcuts: List[Shortcut]) def toHotKeys: String = shortcuts.mkString(",")

type ShortcutCallbacks = PartialFunction[Shortcut, Callback]

given Conversion[PartialFunction[Shortcut, Callback], HotkeysCallback] with
  def apply(p: PartialFunction[Shortcut, Callback]): HotkeysCallback =
    (e: HotkeysEvent) => p.applyOrElse(Shortcut(e.key), _ => Callback.empty)

val GoToObs         = Shortcut("o")
val GoToTargets     = Shortcut("t")
val GoToProposals   = Shortcut("r")
val GoToConstraints = Shortcut("c")
val GoToOverview    = Shortcut("w")

val all: List[Shortcut] =
  List(GoToObs, GoToTargets, GoToProposals, GoToConstraints, GoToOverview)
