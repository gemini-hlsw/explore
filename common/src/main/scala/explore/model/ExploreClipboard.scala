// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.effect.IO
import explore.model.LocalClipboard.*
import org.scalajs.dom

object ExploreClipboard:
  private val storageKey = "clipboard"

  def get: IO[LocalClipboard] = IO {
    // getItem returns null when storage isn't set.
    Option(dom.window.localStorage.getItem(storageKey)).fold(LocalClipboard.Empty)(value =>
      ObsIdSet.fromString
        .getOption(value)
        .map(LocalClipboard.CopiedObservations.apply)
        .orElse(TargetIdSet.fromString.getOption(value).map(LocalClipboard.CopiedTargets.apply))
        .getOrElse(LocalClipboard.Empty)
    )
  }

  def set(item: LocalClipboard): IO[Unit] = IO {
    item match {
      case CopiedObservations(oids) =>
        dom.window.localStorage.setItem(storageKey, ObsIdSet.fromString.reverseGet(oids))
      case CopiedTargets(tids)      =>
        dom.window.localStorage.setItem(storageKey, TargetIdSet.fromString.reverseGet(tids))
      case Empty                    => dom.window.localStorage.removeItem(storageKey)
    }
  }
