// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model.enums

import lucuma.core.util.NewType

/** Visibility of an item. */
type Visible = Visible.Type
object Visible extends NewType[Boolean]:
  val Hidden: Visible = Visible(false)
  val Shown: Visible  = Visible(true)

  extension (self: Visible)
    def fold[A](hidden: => A, shown: => A): A =
      self match
        case Visible.Hidden => hidden
        case Visible.Shown  => shown

    def isVisible: Boolean = fold(false, true)

    def flip: Visible = fold(Visible.Shown, Visible.Hidden)
