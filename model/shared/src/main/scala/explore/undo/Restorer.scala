// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import cats.Eq

// We don't use a case class to avoid the type parameter on T
sealed trait Restorer[F[_], M] { // M = (Local) Model
  type T // T = Value type

  val value: T       // Value that will be restored upon undo/redo
  val getter: M => T // How to refresh the value from the model. Used when going from undo=>redo or viceversa.
  val setter: T => M => M

  val onRestore: (M, T) => F[Unit]

  def onModel(m: M): Restorer[F, M] =
    Restorer[F, M, T](m, getter, setter, onRestore)

  override def toString(): String = s"Restorer($value, ...)"
}

object Restorer {
  def apply[F[_], M, A](
    m:          M,
    _getter:    M => A,
    _setter:    A => M => M,
    _onRestore: (M, A) => F[Unit]
  ): Restorer[F, M] =
    new Restorer[F, M] {

      type T = A

      override val value = _getter(m)

      override val getter = _getter

      override val setter = _setter

      override val onRestore = _onRestore
    }

  implicit def eqRestorer[F[_], M]: Eq[Restorer[F, M]] = Eq.fromUniversalEquals
}
