// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import monocle.Getter
import monocle.Lens
import monocle.Setter

// This is not a Lens. It doesn't abide by any laws. It's just convenience to group a getter and setter functions.
final case class GetSet[T, A](getter: Getter[T, A], setter: Setter[T, A]) {
  lazy val get: T => A             = getter.get
  lazy val set: A => T => T        = setter.set
  lazy val mod: (A => A) => T => T = setter.modify
}
object GetSet {
  def apply[T, A](lens: Lens[T, A]): GetSet[T, A] =
    GetSet(lens.asGetter, lens.asSetter)

  def apply[T, A](get: T => A, set: (A => A) => T => T): GetSet[T, A] =
    GetSet(Getter(get), Setter(set))
}
