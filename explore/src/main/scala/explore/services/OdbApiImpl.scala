// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.effect.IO
import cats.syntax.all.*
import clue.FetchClient
import explore.utils.ToastCtx
import lucuma.schemas.ObservationDB
import org.typelevel.log4cats.Logger

case class OdbApiImpl()(using FetchClient[IO, ObservationDB], Logger[IO], ToastCtx[IO])
    extends OdbApi[IO]
    with OdbTargetApiImpl
