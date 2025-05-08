// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.MonadThrow
import cats.syntax.all.*
import clue.FetchClient
import explore.utils.ToastCtx
import lucuma.schemas.ObservationDB
import org.typelevel.log4cats.Logger

case class OdbApiImpl[F[_]: MonadThrow]()(using
  FetchClient[F, ObservationDB],
  Logger[F],
  ToastCtx[F]
) extends OdbApi[F]
    with OdbTargetApiImpl[F]
    with OdbAsterismApiImpl[F]
    with OdbProgramApiImpl[F]
