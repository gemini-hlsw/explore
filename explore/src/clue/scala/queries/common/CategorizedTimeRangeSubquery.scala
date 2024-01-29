// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import lucuma.core.model.sequence.CategorizedTimeRange
import lucuma.schemas.ObservationDB
import lucuma.odb.json.timeaccounting.given

@GraphQL
object CategorizedTimeRangeSubquery
    extends GraphQLSubquery.Typed[ObservationDB, CategorizedTimeRange]("CatgorizedTimeRange"):
  override val subquery: String = s"""
    {
      minimum $CategorizedTimeSubquery
      maximum $CategorizedTimeSubquery
    }
  """
