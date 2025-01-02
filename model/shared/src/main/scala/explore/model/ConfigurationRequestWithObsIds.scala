// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.Eq
import cats.derived.*
import io.circe.Decoder
import lucuma.core.enums.ConfigurationRequestStatus
import lucuma.core.model.Configuration
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.Observation
import lucuma.odb.json.configurationrequest.query.given
import monocle.Focus
import monocle.Lens

case class ConfigurationRequestWithObsIds(
  id:                     ConfigurationRequest.Id,
  status:                 ConfigurationRequestStatus,
  configuration:          Configuration,
  applicableObservations: List[Observation.Id]
) derives Eq,
      Decoder

object ConfigurationRequestWithObsIds:
  val id: Lens[ConfigurationRequestWithObsIds, ConfigurationRequest.Id]        =
    Focus[ConfigurationRequestWithObsIds](_.id)
  val status: Lens[ConfigurationRequestWithObsIds, ConfigurationRequestStatus] =
    Focus[ConfigurationRequestWithObsIds](_.status)
  val configuration: Lens[ConfigurationRequestWithObsIds, Configuration]       =
    Focus[ConfigurationRequestWithObsIds](_.configuration)
