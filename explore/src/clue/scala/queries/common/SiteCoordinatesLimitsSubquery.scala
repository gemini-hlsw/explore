// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import lucuma.core.model.SiteCoordinatesLimits
import lucuma.schemas.ObservationDB
import lucuma.odb.json.limits.decoder.given

@GraphQL
object SiteCoordinatesLimitsSubquery
    extends GraphQLSubquery.Typed[ObservationDB, SiteCoordinatesLimits](
      "ConfigurationRequest"
    ):
  override val subquery: String = s"""
    {
      raStart {
        microarcseconds
      }
      raEnd {
        microarcseconds
      }
      decStart {
        microarcseconds
      }
      decEnd {
        microarcseconds
      }
    }
  """
