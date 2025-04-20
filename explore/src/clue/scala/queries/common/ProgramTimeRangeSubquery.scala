// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import explore.model.ProgramTimeRange
import lucuma.schemas.ObservationDB

@GraphQL
object ProgramTimeRangeSubquery
    extends GraphQLSubquery.Typed[ObservationDB, ProgramTimeRange]("CategorizedTimeRange"):
  override val subquery: String = s"""
    {
      minimum $ProgramTimeSubquery
      maximum $ProgramTimeSubquery
    }
  """
