// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.MonadThrow
import cats.syntax.all.*
import clue.*
import clue.FetchClient
import clue.data.Input
import clue.syntax.*
import lucuma.core.model.Observation
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.SequenceQueriesGQL.*
import lucuma.ui.sequence.SequenceData

trait OdbSequenceApiImpl[F[_]: MonadThrow](using FetchClient[F, ObservationDB])
    extends OdbSequenceApi[F]:
  def sequenceData(obsId: Observation.Id, includeItc: Boolean): F[Option[SequenceData]] =
    SequenceQuery[F]
      .query(obsId, includeItc = Input(includeItc))
      .raiseGraphQLErrors
      .map(SequenceData.fromOdbResponse)
