// Copyright (c) 2016-2020 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import japgolly.scalajs.react.Callback
import japgolly.scalajs.react.ReactMouseEvent
import cats.effect.Effect
import crystal.react.implicits._

package object react {
  def linkOverride[F[_]: Effect](f: => F[Unit]): ReactMouseEvent => Callback =
    e => linkOverride[F, Unit](f)(Effect[F])(e, ())

  def linkOverride[F[_]: Effect, A](f: => F[Unit]): (ReactMouseEvent, A) => Callback =
    (e: ReactMouseEvent, _: A) => {
      (e.preventDefaultCB *> f.runAsyncCB)
        .unless_(e.ctrlKey || e.metaKey)
    }
}

package react {}
