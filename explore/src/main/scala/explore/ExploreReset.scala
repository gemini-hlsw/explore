// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package cats.effect.unsafe

// Hack to acces `IORuntime.resetGlobal()`, which is package private.
object JSPlatform {
  def resetRuntime(): Unit =
    cats.effect.unsafe.IORuntime.resetGlobal()
}
