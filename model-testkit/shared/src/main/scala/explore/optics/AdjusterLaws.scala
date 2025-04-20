// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.optics

import monocle.internal.IsEq

case class AdjusterLaws[S, A](setter: Adjuster[S, A]) {
  import IsEq.syntax

  def setIdempotent(s: S, a: A): IsEq[S] =
    setter.set(a)(setter.set(a)(s)) <==> setter.set(a)(s)

  def modifyIdentity(s: S): IsEq[S] =
    setter.modify(identity)(s) <==> s

  def consistentSetModify(s: S, a: A): IsEq[S] =
    setter.modify(_ => a)(s) <==> setter.set(a)(s)
}
