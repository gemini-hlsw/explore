// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.cache

import cats.effect.Concurrent
import explore.model.util.SelfUpdatingCache
import lucuma.core.model.Program
import queries.common.TargetQueriesGQL
import explore.DefaultErrorPolicy
import clue.StreamingClient
import lucuma.schemas.ObservationDB
import cats.syntax.all.given
import scala.collection.immutable.SortedMap
import lucuma.core.model.Target
import cats.Order.given
import explore.model.util.Cache

// TODO, Store TargetWithObs???
final case class ModelCaches[F[_]] private (target: Cache[F, SortedMap[Target.Id, Target]])

object ModelCaches:
  private def targetCache[F[_]: Concurrent](programId: Program.Id)(using
    StreamingClient[F, ObservationDB]
  ): F[Cache[F, SortedMap[Target.Id, Target]]] =
    SelfUpdatingCache.init(
      TargetQueriesGQL
        .AllProgramTargets[F]
        .query(programId)
        .map(data =>
          SortedMap.from(
            data.targets.matches.map(targetWithId => targetWithId.id -> targetWithId.target)
          )
        ),
      TargetQueriesGQL.ProgramTargetsDelta
        .subscribe[F](programId)
        .map(_.map(data => _.updated(data.targetEdit.value.id, data.targetEdit.value.target)))
    )

  def forProgram[F[_]: Concurrent](programId: Program.Id)(using
    StreamingClient[F, ObservationDB]
  ): F[ModelCaches[F]] = targetCache(programId).map(ModelCaches(_))
