// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.optics

import explore.optics.all.*
import monocle.Getter
import monocle.Lens

// Wrap a Getter and an Adjuster
case class GetAdjust[T, A](getter: Getter[T, A], adjuster: Adjuster[T, A]) {
  lazy val get: T => A             = getter.get
  lazy val set: A => T => T        = adjuster.set
  lazy val mod: (A => A) => T => T = adjuster.modify

  def andThen[B](lens: Lens[A, B]): GetAdjust[T, B] =
    GetAdjust(getter.andThen(lens), adjuster.andThen(lens))

  def andThen[B](getAdjust: GetAdjust[A, B]): GetAdjust[T, B] =
    GetAdjust(getter.andThen(getAdjust.getter), adjuster.andThen(getAdjust.adjuster))
}
object GetAdjust                                                           {
  def apply[T, A](lens: Lens[T, A]): GetAdjust[T, A] =
    GetAdjust(lens.asGetter, lens.asAdjuster)

  def apply[T, A](get: T => A, mod: (A => A) => T => T): GetAdjust[T, A] =
    GetAdjust(Getter(get), Adjuster(mod))
}
