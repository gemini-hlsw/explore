// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.undo

import monocle.Lens
import cats.Functor

// We don't use a case class to avoid the type parameter on T
sealed trait Restorer[F[_], M] { // M = (Local) Model
  protected implicit val functorF: Functor[F]

  type T // T = Value type

  val value: T           // Value that will be restored upon undo/redo
  val lens: Lens[
    M,
    T
  ]                      // How to refresh the value from the model. Used when going from undo=>redo or viceversa.
  val setM: M => F[Unit] // Modify the model

  def restore(
    m: M
  ): F[Restorer[F, M]] = // Actually restores the value and returns the reverse restorer
    functorF.map(setM(lens.set(value)(m)))(_ => Restorer[F, M, T](m, lens, setM))

  override def toString(): String = s"Restorer($value, ...)"
}

object Restorer {
  def apply[F[_], M, A](m: M, _lens: Lens[M, A], _setM: M => F[Unit])(implicit
    ff:                    Functor[F]
  ): Restorer[F, M] =
    new Restorer[F, M] {
      override protected implicit val functorF = ff

      type T = A

      override val value = _lens.get(m)

      override val lens = _lens

      override val setM = _setM
    }
}
