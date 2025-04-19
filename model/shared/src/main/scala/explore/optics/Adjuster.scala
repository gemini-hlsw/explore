// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.optics

import explore.optics.all.*
import monocle.*

// An Adjuster is just like a Setter, but less strict: it does not abide by the "compose modify" law.
// It allows the modify/set functions to adjust the result (eg: not going out of bounds), which breaks composability.
trait Adjuster[From, To] { self =>
  def set(to: To): From => From

  def modify(f: To => To): From => From

  /**
   * ************************************************************
   */
  /** Compose methods between an [[Adjuster]] and another Optics */
  /**
   * ************************************************************
   */

  /** compose an [[Adjuster]] with a [[Adjuster]] */
  @inline final def andThen[X](other: Adjuster[To, X]): Adjuster[From, X] =
    new Adjuster[From, X] {
      def set(to:   X): From => From      = self.modify(other.set(to))
      def modify(f: X => X): From => From = self.modify(other.modify(f))
    }

  /** compose a [[PAdjuster]] with a [[PAdjuster]] */
  @inline final def andThen[X](other: Setter[To, X]): Adjuster[From, X] =
    andThen(other.asAdjuster)

  /** compose a [[PAdjuster]] with a [[PTraversal]] */
  @inline final def andThen[X](other: Traversal[To, X]): Adjuster[From, X] =
    andThen(other.asAdjuster)

  /** compose a [[PAdjuster]] with a [[POptional]] */
  @inline final def andThen[X](other: Optional[To, X]): Adjuster[From, X] =
    andThen(other.asAdjuster)

  /** compose a [[PAdjuster]] with a [[PPrism]] */
  @inline final def andThen[X](other: Prism[To, X]): Adjuster[From, X] =
    andThen(other.asAdjuster)

  /** compose a [[PAdjuster]] with a [[PLens]] */
  @inline final def andThen[X](other: Lens[To, X]): Adjuster[From, X] =
    andThen(other.asAdjuster)

  /** compose a [[PAdjuster]] with a [[PIso]] */
  @inline final def andThen[X](other: Iso[To, X]): Adjuster[From, X] =
    andThen(other.asAdjuster)

  def asTarget[X](implicit ev: To =:= X): Adjuster[From, X] =
    asInstanceOf[Adjuster[From, X]]
}

object Adjuster {
  def id[X]: Adjuster[X, X] =
    Iso.id[X].asAdjuster

  def apply[From, To](_modify: (To => To) => (From => From)): Adjuster[From, To] =
    new Adjuster[From, To] {
      def set(to:   To): From => From       = modify(_ => to)
      def modify(f: To => To): From => From = _modify(f)
    }
}
