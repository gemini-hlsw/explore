// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.optics

import monocle._

// An Adjuster is just like a Setter, but less strict: it does not abide by the "compose modify" law.
// It allows the modify/set functions to adjust the result (eg: not going out of bounds), which breaks composability.
trait Adjuster[From, To] { self =>
  def set(to: To): From => From

  def modify(f: To => To): From => From

  /** ************************************************************
    */
  /** Compose methods between an [[Adjuster]] and another Optics */
  /** ************************************************************
    */

  /** compose an [[Adjuster]] with a [[Adjuster]] */
  @inline final def composeAdjuster[X](other: Adjuster[To, X]): Adjuster[From, X] =
    new Adjuster[From, X] {
      def set(to:   X): From => From      = self.modify(other.set(to))
      def modify(f: X => X): From => From = self.modify(other.modify(f))
    }

  /** compose a [[PAdjuster]] with a [[PSetter]] */
  @inline final def composeSetter[X](other: Setter[To, X]): Adjuster[From, X] =
    composeAdjuster(other.asAdjuster)

  /** compose a [[PSetter]] with a [[PTraversal]] */
  @inline final def composeTraversal[X](other: Traversal[To, X]): Adjuster[From, X] =
    composeAdjuster(other.asAdjuster)

  /** compose a [[PSetter]] with a [[POptional]] */
  @inline final def composeOptional[X](other: Optional[To, X]): Adjuster[From, X] =
    composeAdjuster(other.asAdjuster)

  /** compose a [[PSetter]] with a [[PPrism]] */
  @inline final def composePrism[X](other: Prism[To, X]): Adjuster[From, X] =
    composeAdjuster(other.asAdjuster)

  /** compose a [[PSetter]] with a [[PLens]] */
  @inline final def composeLens[X](other: Lens[To, X]): Adjuster[From, X] =
    composeAdjuster(other.asAdjuster)

  /** compose a [[PSetter]] with a [[PIso]] */
  @inline final def composeIso[X](other: Iso[To, X]): Adjuster[From, X] =
    composeAdjuster(other.asAdjuster)

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
