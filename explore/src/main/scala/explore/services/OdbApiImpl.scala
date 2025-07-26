// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.effect.Async
import clue.StreamingClient
import explore.utils.ToastCtx
import lucuma.schemas.ObservationDB
import org.typelevel.log4cats.Logger

case class OdbApiImpl[F[_]: Async](
  resetCache:       String => F[Unit],
  notifyFatalError: String => F[Unit]
)(using
  StreamingClient[F, ObservationDB],
  Logger[F],
  ToastCtx[F]
) extends OdbApi[F]
    with OdbApiHelper[F](resetCache, notifyFatalError)
    with OdbTargetApiImpl[F]
    with OdbAsterismApiImpl[F]
    with OdbProgramApiImpl[F]
    with OdbObservationApiImpl[F]
    with OdbGroupApiImpl[F]
    with OdbVisitApiImpl[F]
    with OdbSequenceApiImpl[F]
    with OdbProposalApiImpl[F]
    with OdbConfigApiImpl[F]
