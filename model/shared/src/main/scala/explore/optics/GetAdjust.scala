// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.optics

import monocle.Getter
import monocle.Lens

// Wrap a Getter and an Adjuster
final case class GetAdjust[T, A](getter: Getter[T, A], adjuster: Adjuster[T, A]) {
  lazy val get: T => A             = getter.get
  lazy val set: A => T => T        = adjuster.set
  lazy val mod: (A => A) => T => T = adjuster.modify
}
object GetAdjust {
  def apply[T, A](lens: Lens[T, A]): GetAdjust[T, A] =
    GetAdjust(lens.asGetter, lens.asAdjuster)

  def apply[T, A](get: T => A, mod: (A => A) => T => T): GetAdjust[T, A] =
    GetAdjust(Getter(get), Adjuster(mod))
}
