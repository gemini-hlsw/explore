// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.data.State
import monocle.Lens
import monocle.state.all._

package object utils {

  def abbreviate(s: String, maxLength: Int): String =
    if (s.length > maxLength) s"${s.substring(0, maxLength)}\u2026" else s

  implicit final class LensEditorOps[S, A](self: Lens[S, A]) {
    def edit(a: A): State[S, A]         =
      self.assign(a)
    def :=(a:   A): State[S, A]         =
      edit(a)
    def edit(a: Option[A]): State[S, A] =
      a.fold(self.st)(self.assign)
    def :=(a:   Option[A]): State[S, A] =
      edit(a)
  }
}
