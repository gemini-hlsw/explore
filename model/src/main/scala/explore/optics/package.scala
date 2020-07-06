// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats._
import cats.implicits._
import monocle._
import monocle.std.option.some

package object optics {
  implicit class IsoOps[From, To](val self: Iso[From, To]) extends AnyVal {
    def composeAdjuster[X](other: Adjuster[To, X]): Adjuster[From, X] =
      asAdjuster.composeAdjuster(other)

    @inline final def asAdjuster: Adjuster[From, To] =
      new Adjuster[From, To] {
        def modify(f: To => To): From => From = self.modify(f)
        def set(to:   To): From => From       = self.set(to)
      }
  }

  implicit class LensOps[From, To](val self: Lens[From, To]) extends AnyVal {
    def composeAdjuster[X](other: Adjuster[To, X]): Adjuster[From, X] =
      asAdjuster.composeAdjuster(other)

    @inline final def asAdjuster: Adjuster[From, To] =
      new Adjuster[From, To] {
        def modify(f: To => To): From => From = self.modify(f)
        def set(to:   To): From => From       = self.set(to)
      }
  }

  implicit class PrismOps[From, To](val self: Prism[From, To]) extends AnyVal {
    def composeAdjuster[X](other: Adjuster[To, X]): Adjuster[From, X] =
      asAdjuster.composeAdjuster(other)

    @inline final def asAdjuster: Adjuster[From, To] =
      new Adjuster[From, To] {
        def modify(f: To => To): From => From = self.modify(f)
        def set(to:   To): From => From       = self.set(to)
      }
  }

  implicit class OptionalOps[From, To](val self: Optional[From, To]) extends AnyVal {
    def composeAdjuster[X](other: Adjuster[To, X]): Adjuster[From, X] =
      asAdjuster.composeAdjuster(other)

    @inline final def asAdjuster: Adjuster[From, To] =
      new Adjuster[From, To] {
        def modify(f: To => To): From => From = self.modify(f)
        def set(to:   To): From => From       = self.set(to)
      }
  }

  implicit class TraversalOps[From, To](val self: Traversal[From, To]) extends AnyVal {
    def composeAdjuster[X](other: Adjuster[To, X]): Adjuster[From, X] =
      asAdjuster.composeAdjuster(other)

    @inline final def asAdjuster: Adjuster[From, To] =
      Adjuster(self.modify)
  }

  implicit class SetterOps[From, To](val self: Setter[From, To]) extends AnyVal {
    def composeAdjuster[X](other: Adjuster[To, X]): Adjuster[From, X] =
      asAdjuster.composeAdjuster(other)

    @inline final def asAdjuster: Adjuster[From, To] =
      new Adjuster[From, To] {
        def modify(f: To => To): From => From = self.modify(f)
        def set(to:   To): From => From       = self.set(to)
      }
  }

  implicit class GetterOptionOps[S, A](val getter: Getter[S, Option[A]]) extends AnyVal {
    def composeOptionLens[B](other: Lens[A, B]): Getter[S, Option[B]] =
      Getter(
        getter.composePrism(some).composeLens(other).headOption
      )
  }

  implicit class AdjusterOptionOps[S, A](val setter: Adjuster[S, Option[A]]) extends AnyVal {
    def composeOptionLens[B](other: Lens[A, B]): Adjuster[S, Option[B]] =
      Adjuster { modOptB: (Option[B] => Option[B]) =>
        setter.modify { optA =>
          optA.flatMap[A] { a =>
            modOptB(other.get(a).some).map(b => other.set(b)(a))
          }
        }
      }
  }

  // Lenses must be disjoint (not overlap), or the result will be unsafe.
  // See https://github.com/optics-dev/Monocle/issues/545
  def disjointZip[S, A, B](l1: Lens[S, A], l2: Lens[S, B]): Lens[S, (A, B)] =
    Lens((s: S) => (l1.get(s), l2.get(s)))((ab: (A, B)) =>
      (s: S) => l2.set(ab._2)(l1.set(ab._1)(s))
    )

  def disjointZip[S, A, B, C](l1: Lens[S, A], l2: Lens[S, B], l3: Lens[S, C]): Lens[S, (A, B, C)] =
    Lens((s: S) => (l1.get(s), l2.get(s), l3.get(s)))((abc: (A, B, C)) =>
      (s: S) => l3.set(abc._3)(l2.set(abc._2)(l1.set(abc._1)(s)))
    )
}
