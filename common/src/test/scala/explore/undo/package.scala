// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.Applicative

package undo {
  class VarRef[F[_]: Applicative, A](init: A) {
    var a: A = init

    def get: F[A] = Applicative[F].pure(a)

    def update(f: A => A): F[A] = {
      a = f(a)
      get
    }
  }

  object VarRef {
    def apply[F[_]: Applicative]: Applied[F] = new Applied[F]

    class Applied[F[_]: Applicative] {
      def of[A](init: A): VarRef[F, A] = new VarRef(init)
    }
  }
}
