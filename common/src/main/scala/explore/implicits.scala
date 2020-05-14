// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.ContextShift
import cats.effect.IO
import cats.effect.Timer
import crystal._
import explore.model.Actions
import explore.model.AppContext
import io.chrisdavenport.log4cats.Logger
import japgolly.scalajs.react.Reusability

object implicits extends ShorthandTypes {
  implicit def appContext2ContextShift[F[_]](implicit ctx: AppContext[F]): ContextShift[F] = ctx.cs
  implicit def appContext2Timer[F[_]](implicit ctx:        AppContext[F]): Timer[F]        = ctx.timer
  implicit def appContext2Logger[F[_]](implicit ctx:       AppContext[F]): Logger[F]       = ctx.logger

  implicit class ViewCtxOps[F[_], A](val viewCtx: Ctx[AppContext[F], View[F, A]]) extends AnyVal {
    def actions[I[_[_]]](f: Actions[F] => ActionInterpreter[F, I, A]): I[F] =
      viewCtx.interpreter(ctx => f(ctx.actions))
  }

  implicit class ViewOptCtxOps[F[_], A](val viewCtx: Ctx[AppContext[F], ViewOpt[F, A]])
      extends AnyVal {
    def actions[I[_[_]]](f: Actions[F] => ActionInterpreterOpt[F, I, A]): I[F] =
      viewCtx.interpreter(ctx => f(ctx.actions))
  }

  @inline def ViewCtxIOReusability[A](implicit
    r: Reusability[A]
  ): Reusability[Ctx[AppContext[IO], View[IO, A]]] =
    crystal.react.implicits.viewCtxReusability[IO, AppContext[IO], A]

  @inline def ViewCtxOptReusability[A](implicit
    r: Reusability[A]
  ): Reusability[Ctx[AppContext[IO], ViewOpt[IO, A]]] =
    crystal.react.implicits.viewOptCtxReusability[IO, AppContext[IO], A]
}
