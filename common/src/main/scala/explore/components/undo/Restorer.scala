package explore.components.undo

import cats.effect.IO
import monocle.Getter

// We don't use a case class to avoid the type parameter on T
trait Restorer[M] { // M = (Local) Model
  type T // T = Value type

  val value: T // Value that will be restored upon undo/redo
  val getter: Getter[
    M,
    T
  ] // How to refresh the value from the model. Used when going from undo=>redo or viceversa.
  val setter: T => IO[Unit] // Modify the model

  def restore(m: M): IO[Restorer[M]] = // Actually restores the value and returns the reverse restorer
    setter(value).map(_ => Restorer[M, T](m, getter, setter))

  override def toString(): String = s"Restorer($value, ...)"
}

object Restorer {
  def apply[M, A](m: M, _getter: Getter[M, A], _setter: A => IO[Unit]): Restorer[M] =
    new Restorer[M] {
      type T = A

      override val value = _getter.get(m)

      override val getter = _getter

      override val setter = _setter
    }
}
