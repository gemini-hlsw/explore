// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.effect.Resource
import lucuma.core.model.Observation
import lucuma.schemas.model.ExecutionVisits
import queries.common.VisitQueriesGQL.*

trait OdbVisitApi[F[_]]:
  def observationVisits(obsId: Observation.Id): F[Option[ExecutionVisits]]
  def stepEventSubscription(
    obsId: Observation.Id
  ): Resource[F, fs2.Stream[F, StepEventSubscription.Data]]
  def datasetEventSubscription(
    obsId: Observation.Id
  ): Resource[F, fs2.Stream[F, DatasetEditSubscription.Data]]
