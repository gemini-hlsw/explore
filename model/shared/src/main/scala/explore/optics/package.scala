// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.syntax.all._
import monocle._
import monocle.function.At
import monocle.function.At.atMap
import monocle.function.Index
import monocle.function.Index.fromAt

import scala.collection.immutable.TreeSeqMap

package object optics extends ModelOptics {
  implicit class IsoOps[From, To](val self: Iso[From, To]) extends AnyVal {
    def andThen[X](other: Adjuster[To, X]): Adjuster[From, X] =
      asAdjuster.andThen(other)

    @inline final def asAdjuster: Adjuster[From, To] =
      new Adjuster[From, To] {
        def modify(f: To => To): From => From = self.modify(f)
        def set(to: To): From => From         = self.replace(to)
      }
  }

  implicit class LensOps[From, To](val self: Lens[From, To]) extends AnyVal {
    def andThen[X](other: Adjuster[To, X]): Adjuster[From, X] =
      asAdjuster.andThen(other)

    @inline final def asAdjuster: Adjuster[From, To] =
      new Adjuster[From, To] {
        def modify(f: To => To): From => From = self.modify(f)
        def set(to: To): From => From         = self.replace(to)
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
        def set(to: To): From => From         = self.replace(to)
      }
  }

  implicit class OptionalOps[From, To](val self: Optional[From, To]) extends AnyVal {
    def andThen[X](other: Adjuster[To, X]): Adjuster[From, X] =
      asAdjuster.andThen(other)

    @inline final def asAdjuster: Adjuster[From, To] =
      new Adjuster[From, To] {
        def modify(f: To => To): From => From = self.modify(f)
        def set(to: To): From => From         = self.replace(to)
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
        def set(to: To): From => From         = self.replace(to)
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
  }

  implicit class GetterOptionOps[S, A](val getter: Getter[S, Option[A]]) extends AnyVal {
    def composeOptionLens[B](other: Lens[A, B]): Getter[S, Option[B]] =
      composeOptionGetter(other.asGetter)

    def composeOptionGetter[B](other: Getter[A, B]): Getter[S, Option[B]] =
      Getter(
        getter.some.andThen(other).headOption
      )
  }

  implicit class AdjusterOptionOps[S, A](val setter: Adjuster[S, Option[A]]) extends AnyVal {
    def composeOptionLens[B](other: Lens[A, B]): Adjuster[S, Option[B]] =
      composeOptionGetAdjust(other.asGetAdjust)

    def composeOptionGetAdjust[B](other: GetAdjust[A, B]): Adjuster[S, Option[B]] =
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
      (s: S) => l2.replace(ab._2)(l1.replace(ab._1)(s))
    )

  def disjointZip[S, A, B, C](l1: Lens[S, A], l2: Lens[S, B], l3: Lens[S, C]): Lens[S, (A, B, C)] =
    Lens((s: S) => (l1.get(s), l2.get(s), l3.get(s)))((abc: (A, B, C)) =>
      (s: S) => l3.replace(abc._3)(l2.replace(abc._2)(l1.replace(abc._1)(s)))
    )

  def disjointZip[S, A, B, C, D](
    l1: Lens[S, A],
    l2: Lens[S, B],
    l3: Lens[S, C],
    l4: Lens[S, D]
  ): Lens[S, (A, B, C, D)] =
    Lens((s: S) => (l1.get(s), l2.get(s), l3.get(s), l4.get(s)))((abc: (A, B, C, D)) =>
      (s: S) => l4.replace(abc._4)(l3.replace(abc._3)(l2.replace(abc._2)(l1.replace(abc._1)(s))))
    )

  def optionIso[A, B](iso: Iso[A, B]): Iso[Option[A], Option[B]] =
    Iso[Option[A], Option[B]](_.map(iso.get))(_.map(iso.reverseGet))

  def getWithDefault[A](default: => A): Lens[Option[A], A] =
    Lens[Option[A], A](_.getOrElse(default))(a => _ => a.some)

  // This should be safe to use with Maps that have .withDefault(...)
  def atMapWithDefault[K, V](k: K, default: => V): Lens[Map[K, V], V] =
    atMap.at(k).andThen(getWithDefault(default))

  implicit def atTreeSeqMap[K, V]: At[TreeSeqMap[K, V], K, Option[V]] =
    At(i =>
      Lens((_: TreeSeqMap[K, V]).get(i))(optV => map => optV.fold(map - i)(v => map + (i -> v)))
    )

  implicit def indexTreeSeqMap[K, V]: Index[TreeSeqMap[K, V], K, V] =
    fromAt(atTreeSeqMap)

}
