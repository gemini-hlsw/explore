// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import lucuma.core.model.SiteCoordinatesLimits
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.RASubquery
import lucuma.schemas.odb.DecSubquery
import lucuma.odb.json.limits.decoder.given

@GraphQL
object SiteCoordinatesLimitsSubquery
    extends GraphQLSubquery.Typed[ObservationDB, SiteCoordinatesLimits](
      "CoordinatesLimits"
    ):
  override val subquery: String = s"""
    {
      raStart $RASubquery
      raEnd $RASubquery
      decStart $DecSubquery
      decEnd $DecSubquery
    }
  """
