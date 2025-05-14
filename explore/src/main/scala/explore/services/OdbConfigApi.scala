// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.services

import cats.effect.Resource
import explore.model.ConfigurationRequestWithObsIds
import explore.modes.ScienceModes
import lucuma.core.model.Program

trait OdbConfigApi[F[_]]:
  def scienceModes: F[ScienceModes]
  def allProgramConfigurationRequests(
    programId: Program.Id
  ): F[List[ConfigurationRequestWithObsIds]]
  def programConfigurationRequestsDeltaSubscription(
    programId: Program.Id
  ): Resource[F, fs2.Stream[F, ConfigurationRequestWithObsIds]]
