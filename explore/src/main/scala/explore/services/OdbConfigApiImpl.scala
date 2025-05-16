// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.MonadThrow
import cats.effect.Resource
import cats.syntax.all.*
import clue.StreamingClient
import clue.data.syntax.*
import clue.model.GraphQLResponse.*
import explore.model.ConfigurationRequestWithObsIds
import explore.model.SupportedInstruments
import explore.modes.ImagingModeRow
import explore.modes.ImagingModesMatrix
import explore.modes.ScienceModes
import explore.modes.SpectroscopyModeRow
import explore.modes.SpectroscopyModesMatrix
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Program
import lucuma.schemas.ObservationDB
import lucuma.schemas.ObservationDB.Types.ConfigurationRequestEditInput
import org.typelevel.log4cats.Logger
import queries.common.ModesQueriesGQL
import queries.common.ProgramQueriesGQL.ConfigurationRequestSubscription
import queries.common.ProgramSummaryQueriesGQL.AllProgramConfigurationRequests

trait OdbConfigApiImpl[F[_]: MonadThrow](using
  StreamingClient[F, ObservationDB],
  Logger[F]
) extends OdbConfigApi[F]:
  self: OdbApiHelper[F] =>

  def scienceModes: F[ScienceModes] =
    ModesQueriesGQL
      .ScienceModes[F]
      .query(SupportedInstruments)
      .processErrors
      .map: u =>
        val imgModes: List[ImagingModeRow]       =
          u.imagingConfigOptions.zipWithIndex.map: (s, i) =>
            s.copy(id = i.some)
        val img                                  = ImagingModesMatrix(imgModes)
        val specModes: List[SpectroscopyModeRow] =
          u.spectroscopyConfigOptions.zipWithIndex.map: (s, i) =>
            s.copy(id = i.some)
        val spec                                 = SpectroscopyModesMatrix(specModes)
        ScienceModes(spec, img)

  def allProgramConfigurationRequests(
    programId: Program.Id
  ): F[List[ConfigurationRequestWithObsIds]] =
    drain[
      ConfigurationRequestWithObsIds,
      ConfigurationRequest.Id,
      Option[AllProgramConfigurationRequests.Data.Program.ConfigurationRequests]
    ](
      offset =>
        AllProgramConfigurationRequests[F]
          .query(programId, offset.orUnassign)
          .processErrors
          .map(_.program.map(_.configurationRequests)),
      _.foldMap(_.matches),
      _.fold(false)(_.hasMore),
      _.id
    )

  def programConfigurationRequestsDeltaSubscription(
    programId: Program.Id
  ): Resource[F, fs2.Stream[F, ConfigurationRequestWithObsIds]] =
    ConfigurationRequestSubscription
      .subscribe[F](ConfigurationRequestEditInput(programId.assign))
      .logGraphQLErrors(_ => "Error in ConfigurationRequestSubscription subscription")
      .map(_.map(_.configurationRequestEdit.configurationRequest).unNone)
