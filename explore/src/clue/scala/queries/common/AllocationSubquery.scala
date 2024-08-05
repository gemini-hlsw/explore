// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import explore.model.Allocation
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.TimeSpanSubquery

@GraphQL
object AllocationSubquery extends GraphQLSubquery.Typed[ObservationDB, Allocation]("Allocation"):
  override val subquery: String = s"""
    {
      category
      scienceBand
      duration $TimeSpanSubquery
    }
  """
