// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.Applicative
import crystal.ViewF
import japgolly.scalajs.react.util.DefaultEffects.{Sync => DefaultS}

package object undo                           {
  type View[A] = ViewF[DefaultS, A]

  object View {
    @inline
    def apply[A](
      value: A,
      modCB: ((A => A), A => DefaultS[Unit]) => DefaultS[Unit]
    ): ViewF[DefaultS, A] = ViewF[DefaultS, A](value, modCB)
  }
}

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
