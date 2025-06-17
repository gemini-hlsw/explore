// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import lucuma.core.model.ObservationWorkflow
import lucuma.core.util.CalculatedValue
import lucuma.schemas.ObservationDB
import lucuma.schemas.decoders.given

@GraphQL
object CalculatedObservationWorkflowSubquery
    extends GraphQLSubquery.Typed[ObservationDB, CalculatedValue[ObservationWorkflow]](
      "CalculatedObservationWorkflow"
    ):
  override val subquery: String = s"""
    {
      state
      value $ObservationWorkflowSubquery
    }
  """
