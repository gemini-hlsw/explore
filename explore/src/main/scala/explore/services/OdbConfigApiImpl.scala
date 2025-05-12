// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.MonadThrow
import cats.syntax.all.*
import clue.FetchClient
import clue.model.GraphQLResponse.*
import explore.model.SupportedInstruments
import explore.modes.SpectroscopyModeRow
import explore.modes.SpectroscopyModesMatrix
import lucuma.schemas.ObservationDB
import queries.common.ModesQueriesGQL

trait OdbConfigApiImpl[F[_]: MonadThrow](using FetchClient[F, ObservationDB])
    extends OdbConfigApi[F]:
  def spectroscopyModes: F[SpectroscopyModesMatrix] =
    ModesQueriesGQL
      .SpectroscopyModes[F]
      .query(SupportedInstruments)
      .raiseGraphQLErrors
      .map: u =>
        val modes: List[SpectroscopyModeRow] =
          u.spectroscopyConfigOptions.zipWithIndex.map: (s, i) =>
            s.copy(id = i.some)
        SpectroscopyModesMatrix(modes)
