// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import cats.effect.Sync
import cats.syntax.all.given
import clue.FetchClient
import explore.DefaultErrorPolicy
import explore.utils.*
import lucuma.core.model.Program
import lucuma.core.model.Target
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.input.*
import queries.common.TargetQueriesGQL

object TargetQueries:
  def insertTarget[F[_]: Sync](
    programId: Program.Id,
    target:    Target.Sidereal
  )(using
    FetchClient[F, ObservationDB],
    ToastCtx[F]
  ): F[Target.Id] =
    TargetQueriesGQL
      .CreateTargetMutation[F]
      .execute(target.toCreateTargetInput(programId))
      .map(_.createTarget.target.id)
      .flatTap(id => ToastCtx[F].showToast(s"Created new target [$id]"))
