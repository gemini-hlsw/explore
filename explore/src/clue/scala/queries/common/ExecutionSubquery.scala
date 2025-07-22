// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import explore.model.Execution
import lucuma.schemas.ObservationDB

@GraphQL
object ExecutionSubquery extends GraphQLSubquery.Typed[ObservationDB, Execution]("Execution") {
  override val subquery: String = s"""
    {
      digest $CalculatedDigestSubquery
      timeCharge $ProgramTimeSubquery
    }
  """
}
