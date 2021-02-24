// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.components.ui

import cats.syntax.all._
import react.common.implicits._
import react.common.style._

object FomanticStyles {
  val Text: Css = Css("text")

  val Negative: Css = Css("negative")
  val Error: Css    = Negative
  val Warning: Css  = Css("warning")

  val ErrorText: Css   = Text |+| Error
  val WarningText: Css = Text |+| Warning

  val Divider: Css = Css("ui divider")

  val Table: Css        = Css("ui table")
  val Compact: Css      = Css("compact")
  val CompactTable: Css = Table |+| Compact

  val Aligned: Css      = Css("aligned")
  val RightAligned: Css = Css("right") |+| Aligned
}
