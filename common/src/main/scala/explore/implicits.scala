// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore

import cats.effect.ContextShift
import cats.effect.Timer
import explore.model.AppContext
import io.chrisdavenport.log4cats.Logger

object implicits extends ShorthandTypes {
  implicit def appContext2ContextShift[F[_]](implicit ctx: AppContext[F]): ContextShift[F] = ctx.cs
  implicit def appContext2Timer[F[_]](implicit ctx:        AppContext[F]): Timer[F]        = ctx.timer
  implicit def appContext2Logger[F[_]](implicit ctx:       AppContext[F]): Logger[F]       = ctx.logger
}
