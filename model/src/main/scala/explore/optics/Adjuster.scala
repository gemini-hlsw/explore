// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.optics

import cats.Contravariant
import cats.Functor
import cats.arrow.Choice
import cats.arrow.Profunctor
import cats.syntax.either._
import monocle._

// An Adjuster is just like a Setter, but weaker: it does not abide by the "compose modify" law.
// It allows the modify/set functions to adjust the result (eg: not going out of bounds), which breaks composability.
abstract class PAdjuster[S, T, A, B] extends Serializable { self =>

  /** modify polymorphically the target of a [[PAdjuster]] with a function */
  def modify(f: A => B): S => T

  /** set polymorphically the target of a [[PAdjuster]] with a value */
  def set(b: B): S => T

  /** join two [[PAdjuster]] with the same target */
  @inline final def choice[S1, T1](
    other: PAdjuster[S1, T1, A, B]
  ): PAdjuster[Either[S, S1], Either[T, T1], A, B] =
    PAdjuster[Either[S, S1], Either[T, T1], A, B](b => _.bimap(self.modify(b), other.modify(b)))

  /*************************************************************/
  /** Compose methods between a [[PAdjuster]] and another Optics */
  /*************************************************************/
  /** compose a [[PAdjuster]] with a [[PAdjuster]] */
  @inline final def composeAdjuster[C, D](other: PAdjuster[A, B, C, D]): PAdjuster[S, T, C, D] =
    new PAdjuster[S, T, C, D] {
      def modify(f: C => D): S => T =
        self.modify(other.modify(f))

      def set(d: D): S => T =
        self.modify(other.set(d))
    }

  /** compose a [[PAdjuster]] with a [[PSetter]] */
  @inline final def composeSetter[C, D](other: PSetter[A, B, C, D]): PAdjuster[S, T, C, D] =
    composeAdjuster(other.asAdjuster)

  /** compose a [[PSetter]] with a [[PTraversal]] */
  @inline final def composeTraversal[C, D](other: PTraversal[A, B, C, D]): PAdjuster[S, T, C, D] =
    composeAdjuster(other.asAdjuster)

  /** compose a [[PSetter]] with a [[POptional]] */
  @inline final def composeOptional[C, D](other: POptional[A, B, C, D]): PAdjuster[S, T, C, D] =
    composeAdjuster(other.asAdjuster)

  /** compose a [[PSetter]] with a [[PPrism]] */
  @inline final def composePrism[C, D](other: PPrism[A, B, C, D]): PAdjuster[S, T, C, D] =
    composeAdjuster(other.asAdjuster)

  /** compose a [[PSetter]] with a [[PLens]] */
  @inline final def composeLens[C, D](other: PLens[A, B, C, D]): PAdjuster[S, T, C, D] =
    composeAdjuster(other.asAdjuster)

  /** compose a [[PSetter]] with a [[PIso]] */
  @inline final def composeIso[C, D](other: PIso[A, B, C, D]): PAdjuster[S, T, C, D] =
    composeAdjuster(other.asAdjuster)

  /********************************************/
  /** Experimental aliases of compose methods */
  /********************************************/
  /** alias to composeTraversal */
  @inline final def ^|->>[C, D](other: PTraversal[A, B, C, D]): PAdjuster[S, T, C, D] =
    composeTraversal(other)

  /** alias to composeOptional */
  @inline final def ^|-?[C, D](other: POptional[A, B, C, D]): PAdjuster[S, T, C, D] =
    composeOptional(other)

  /** alias to composePrism */
  @inline final def ^<-?[C, D](other: PPrism[A, B, C, D]): PAdjuster[S, T, C, D] =
    composePrism(other)

  /** alias to composeLens */
  @inline final def ^|->[C, D](other: PLens[A, B, C, D]): PAdjuster[S, T, C, D] =
    composeLens(other)

  /** alias to composeIso */
  @inline final def ^<->[C, D](other: PIso[A, B, C, D]): PAdjuster[S, T, C, D] =
    composeIso(other)
}

object PAdjuster {
  def id[S, T]: PAdjuster[S, T, S, T] =
    PIso.id[S, T].asAdjuster

  def codiagonal[S, T]: PAdjuster[Either[S, S], Either[T, T], S, T] =
    PAdjuster[Either[S, S], Either[T, T], S, T](f => _.bimap(f, f))

  /** create a [[PSetter]] using modify function */
  def apply[S, T, A, B](_modify: (A => B) => S => T): PAdjuster[S, T, A, B] =
    new PAdjuster[S, T, A, B] {
      def modify(f: A => B): S => T =
        _modify(f)

      def set(b: B): S => T =
        _modify(_ => b)
    }

  /** create a [[PSetter]] from a cats.Functor */
  def fromFunctor[F[_], A, B](implicit F: Functor[F]): PAdjuster[F[A], F[B], A, B] =
    PAdjuster[F[A], F[B], A, B](f => F.map(_)(f))

  /** create a [[PSetter]] from a Contravariant functor */
  def fromContravariant[F[_], A, B](implicit F: Contravariant[F]): PAdjuster[F[B], F[A], A, B] =
    PAdjuster[F[B], F[A], A, B](f => F.contramap(_)(f))

  /** create a [[PSetter]] from a Profunctor */
  def fromProfunctor[P[_, _], A, B, C](implicit
    P: Profunctor[P]
  ): PAdjuster[P[B, C], P[A, C], A, B] =
    PAdjuster[P[B, C], P[A, C], A, B](f => P.lmap(_)(f))
}

object Adjuster {
  def id[A]: Adjuster[A, A] =
    Iso.id[A].asAdjuster

  def codiagonal[S]: Adjuster[Either[S, S], S] =
    PAdjuster.codiagonal

  /** [[Adjuster]] that points to nothing */
  def void[S, A]: Adjuster[S, A] =
    Optional.void.asAdjuster

  /** alias for [[Adjuster]] apply with a monomorphic modify function */
  def apply[S, A](modify: (A => A) => S => S): Adjuster[S, A] =
    PAdjuster(modify)
}
