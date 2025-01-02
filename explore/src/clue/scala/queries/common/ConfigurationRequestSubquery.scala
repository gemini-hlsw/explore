// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import explore.model.ConfigurationRequestWithObsIds
import lucuma.schemas.ObservationDB

@GraphQL
object ConfigurationRequestSubquery
    extends GraphQLSubquery.Typed[ObservationDB, ConfigurationRequestWithObsIds](
      "ConfigurationRequest"
    ):
  override val subquery: String = s"""
    {
      id
      status
      configuration $ConfigurationSubquery
      applicableObservations
    }
  """
