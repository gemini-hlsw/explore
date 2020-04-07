// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import crystal.ViewCtx
import explore.model.AppContext
import cats.effect.IO
import cats.effect.ContextShift
import cats.effect.Timer
import explore.model.Actions
import crystal.ActionInterpreter
import japgolly.scalajs.react.Reusability

object implicits {
  type Eff[A]      = IO[A]
  type AppContextF = AppContext[Eff]
  type ViewCtxF[A] = ViewCtx[Eff, AppContextF, A]

  implicit def appContext2ContextShift[F[_]](implicit ctx: AppContext[F]): ContextShift[F] = ctx.cs
  implicit def appContext2Timer[F[_]](implicit ctx:        AppContext[F]): Timer[F]        = ctx.timer

  implicit class ViewCtxOps[F[_], A](val viewCtx: ViewCtx[F, AppContext[F], A]) extends AnyVal {
    def actions[I[_[_]]](f: Actions[F] => ActionInterpreter[F, I, A]): I[F] =
      viewCtx.interpreter(ctx => f(ctx.actions))
  }

  @inline def ViewCtxFReusability[A](implicit r: Reusability[A]): Reusability[ViewCtxF[A]] =
    crystal.react.implicits.viewCtxReusability[Eff, AppContextF, A]
}
