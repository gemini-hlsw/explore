// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.MonadThrow
import cats.effect.IO
import japgolly.scalajs.react.React
import japgolly.scalajs.react.feature.Context
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB.Types.UpdateTargetsInput

trait OdbApi[F[_]: MonadThrow]:
  private val Uninitialized: F[Unit] =
    MonadThrow[F].raiseError:
      new RuntimeException("OdbApi is not initialized.")

  def updateTarget(targetId: Target.Id, input: UpdateTargetsInput): F[Unit] = Uninitialized

object OdbApi:
  // Default value noop implementations with warning
  val ctx: Context[OdbApi[IO]] = React.createContext(new OdbApi[IO] {})
