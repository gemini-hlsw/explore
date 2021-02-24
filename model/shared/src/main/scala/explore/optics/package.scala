// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.syntax.all._
import coulomb._
import explore.model.utils._
import lucuma.core.math.ApparentRadialVelocity
import lucuma.core.math.Constants._
import lucuma.core.math.ProperMotion
import lucuma.core.math.RadialVelocity
import lucuma.core.math.Redshift
import lucuma.core.math.units._
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

  implicit class GetAdjustOptionOps[S, A](val getAdjust: GetAdjust[S, Option[A]]) extends AnyVal {
    def composeOptionLens[B](other: Lens[A, B]): GetAdjust[S, Option[B]] =
      GetAdjust(getAdjust.getter.composeOptionLens(other),
                getAdjust.adjuster.composeOptionLens(other)
      )
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

  val unsafePMDecLensO: Lens[Option[ProperMotion], Option[ProperMotion.Dec]] =
    Lens[Option[ProperMotion], Option[ProperMotion.Dec]](_.map(ProperMotion.dec.get))(s =>
      a => buildProperMotion(a.map(_.ra), s)
    )

  val unsafePMRALensO: Lens[Option[ProperMotion], Option[ProperMotion.RA]] =
    Lens[Option[ProperMotion], Option[ProperMotion.RA]](_.map(ProperMotion.ra.get))(s =>
      a => buildProperMotion(s, a.map(_.dec))
    )

  val fromKilometersPerSecondCZ: Iso[BigDecimal, ApparentRadialVelocity] =
    Iso[BigDecimal, ApparentRadialVelocity](b =>
      ApparentRadialVelocity(b.withUnit[KilometersPerSecond])
    )(cz => cz.cz.toUnit[KilometersPerSecond].value)

  val redshiftBigDecimalISO: Iso[BigDecimal, Redshift] = Iso(Redshift.apply)(_.z)

  val fromKilometersPerSecondRV: Prism[BigDecimal, RadialVelocity] =
    Prism[BigDecimal, RadialVelocity](b =>
      Some(b)
        .filter(_.abs <= SpeedOfLight.to[BigDecimal, KilometersPerSecond].value)
        .flatMap(v => RadialVelocity(v.withUnit[KilometersPerSecond]))
    )(rv => rv.rv.toUnit[KilometersPerSecond].value)

  // Iso for coulumb quantities
  def coulombIso[N, U] = Iso[Quantity[N, U], N](_.value)(_.withUnit[U])

}
