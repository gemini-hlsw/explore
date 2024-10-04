// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.optics

import cats.syntax.all.*
import monocle.*
import monocle.function.At
import monocle.function.At.atMap

object all extends ModelOptics {

  implicit class IsoOps[From, To](val self: Iso[From, To]) extends AnyVal {
    def andThen[X](other: Adjuster[To, X]): Adjuster[From, X] =
      asAdjuster.andThen(other)

    @inline final def asAdjuster: Adjuster[From, To] =
      new Adjuster[From, To] {
        def modify(f: To => To): From => From = self.modify(f)
        def set(to:   To): From => From       = self.replace(to)
      }
  }

  implicit class LensOps[From, To](val self: Lens[From, To]) extends AnyVal {
    def andThen[X](other: Adjuster[To, X]): Adjuster[From, X] =
      asAdjuster.andThen(other)

    @inline final def asAdjuster: Adjuster[From, To] =
      new Adjuster[From, To] {
        def modify(f: To => To): From => From = self.modify(f)
        def set(to:   To): From => From       = self.replace(to)
      }

    def composeGetAdjust[X](other: GetAdjust[To, X]): GetAdjust[From, X] =
      new GetAdjust[From, X](self.asGetter.andThen(other.getter),
                             asAdjuster.andThen(other.adjuster)
      )

    @inline final def asGetAdjust: GetAdjust[From, To] =
      new GetAdjust[From, To](self.asGetter, asAdjuster)
  }

  implicit class PrismOps[From, To](val self: Prism[From, To]) extends AnyVal {
    def andThen[X](other: Adjuster[To, X]): Adjuster[From, X] =
      asAdjuster.andThen(other)

    @inline final def asAdjuster: Adjuster[From, To] =
      new Adjuster[From, To] {
        def modify(f: To => To): From => From = self.modify(f)
        def set(to:   To): From => From       = self.replace(to)
      }
  }

  implicit class OptionalOps[From, To](val self: Optional[From, To]) extends AnyVal {
    def andThen[X](other: Adjuster[To, X]): Adjuster[From, X] =
      asAdjuster.andThen(other)

    @inline final def asAdjuster: Adjuster[From, To] =
      new Adjuster[From, To] {
        def modify(f: To => To): From => From = self.modify(f)
        def set(to:   To): From => From       = self.replace(to)
      }
  }

  implicit class TraversalOps[From, To](val self: Traversal[From, To]) extends AnyVal {
    def composeAdjuster[X](other: Adjuster[To, X]): Adjuster[From, X] =
      asAdjuster.andThen(other)

    @inline final def asAdjuster: Adjuster[From, To] =
      Adjuster(self.modify)
  }

  implicit class SetterOps[From, To](val self: Setter[From, To]) extends AnyVal {
    def andThen[X](other: Adjuster[To, X]): Adjuster[From, X] =
      asAdjuster.andThen(other)

    @inline final def asAdjuster: Adjuster[From, To] =
      new Adjuster[From, To] {
        def modify(f: To => To): From => From = self.modify(f)
        def set(to:   To): From => From       = self.replace(to)
      }
  }

  implicit class GetAdjustOptionOps[S, A](val getAdjust: GetAdjust[S, Option[A]]) extends AnyVal {
    def composeOptionLens[B](other: Lens[A, B]): GetAdjust[S, Option[B]] =
      GetAdjust(getAdjust.getter.composeOptionLens(other),
                getAdjust.adjuster.composeOptionLens(other)
      )

    def composeOptionGetAdjust[B](other: GetAdjust[A, B]): GetAdjust[S, Option[B]] =
      GetAdjust(getAdjust.getter.composeOptionGetter(other.getter),
                getAdjust.adjuster.composeOptionGetAdjust(other)
      )

    def composeOptionOptionLens[B](other: Lens[A, Option[B]]): GetAdjust[S, Option[B]] =
      composeOptionOptionGetAdjust(other.asGetAdjust)

    def composeOptionOptionGetAdjust[B](other: GetAdjust[A, Option[B]]): GetAdjust[S, Option[B]] =
      GetAdjust(getAdjust.getter.composeOptionOptionGetter(other.getter),
                getAdjust.adjuster.composeOptionOptionGetAdjust(other)
      )
  }

  implicit class GetterOptionOps[S, A](val getter: Getter[S, Option[A]]) extends AnyVal {
    def composeOptionLens[B](other: Lens[A, B]): Getter[S, Option[B]] =
      composeOptionGetter(other.asGetter)

    def composeOptionGetter[B](other: Getter[A, B]): Getter[S, Option[B]] =
      Getter(
        getter.some.andThen(other).headOption
      )

    def composeOptionOptionGetter[B](other: Getter[A, Option[B]]): Getter[S, Option[B]] =
      Getter(
        getter.some.andThen(other.some).headOption
      )
  }

  implicit class AdjusterOptionOps[S, A](val setter: Adjuster[S, Option[A]]) extends AnyVal {
    def composeOptionLens[B](other: Lens[A, B]): Adjuster[S, Option[B]] =
      composeOptionGetAdjust(other.asGetAdjust)

    def composeOptionGetAdjust[B](other: GetAdjust[A, B]): Adjuster[S, Option[B]] =
      Adjuster { (modOptB: (Option[B] => Option[B])) =>
        setter.modify { optA =>
          optA.flatMap[A] { a =>
            modOptB(other.get(a).some).map(b => other.set(b)(a))
          }
        }
      }

    def composeOptionOptionLens[B](other: Lens[A, Option[B]]): Adjuster[S, Option[B]] =
      composeOptionOptionGetAdjust(other.asGetAdjust)

    def composeOptionOptionGetAdjust[B](other: GetAdjust[A, Option[B]]): Adjuster[S, Option[B]] =
      Adjuster { (modOptB: (Option[B] => Option[B])) =>
        setter.modify { optA =>
          optA.flatMap[A] { a =>
            modOptB(other.get(a)).map(b => other.set(b.some)(a))
          }
        }
      }
  }

  def getWithDefault[A](default: => A): Lens[Option[A], A] =
    Lens[Option[A], A](_.getOrElse(default))(a => _ => a.some)

  // This should be safe to use with Maps that have .withDefault(...)
  def atMapWithDefault[K, V](k: K, default: => V): Lens[Map[K, V], V] =
    atMap.at(k).andThen(getWithDefault(default))
}
