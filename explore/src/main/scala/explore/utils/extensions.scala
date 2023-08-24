// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.utils

import cats.effect.IO
import crystal.react.syntax.effect.*
import japgolly.scalajs.react.Callback
import lucuma.react.primereact.Message
import org.typelevel.log4cats.Logger

extension (f: Callback)
  def showToastCB(text: String, severity: Message.Severity = Message.Severity.Info)(using
    ToastCtx[IO],
    Logger[IO]
  ): Callback =
    f.toAsync.withToast(text, severity).runAsync
