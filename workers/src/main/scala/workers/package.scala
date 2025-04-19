// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package workers

import cats.effect.Async
import cats.effect.IO
import cats.~>
import japgolly.scalajs.react.callback.AsyncCallback
import japgolly.scalajs.react.callback.Callback

import scalajs.js

// Copied and generalized from CallbackCatsEffect
// Move all of these somewhere else?
class AsyncCallbackToF[F[_]](dispatch: Callback => Unit)(using F: Async[F])
    extends (AsyncCallback ~> F) {
  override def apply[A](f: AsyncCallback[A]): F[A] =
    F.async[A](k =>
      F.delay {
        val s = new AsyncCallback.State
        val g = f.underlyingRepr(s)
        val d = g(t => Callback(k(t.toEither)))
        dispatch(d)
        s.cancelCallback.map(x => F.delay(x.runNow()))
      }
    )
}

extension [A](self: AsyncCallback[A])
  def toF[F[_]](using Async[F]): F[A] =
    new AsyncCallbackToF(_.async.toCallback.runNow())(self)

trait PromiseConverter[F[_]]:
  def convert[A](promise: => js.Promise[A]): F[A]

object PromiseConverter:
  def apply[F[_]](using ev: PromiseConverter[F]) = ev

given PromiseConverter[IO] = new PromiseConverter[IO] {
  inline def convert[A](promise: => js.Promise[A]): IO[A] = IO.fromPromise(IO(promise))
}
