// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import lucuma.core.model.ObservationWorkflow
import lucuma.schemas.ObservationDB

@GraphQL
object ObservationWorkflowSubquery
    extends GraphQLSubquery.Typed[ObservationDB, ObservationWorkflow]("ObservationWorkflow"):
  override val subquery: String = s"""
    {
      state
      validTransitions
      validationErrors $ObservationValidationsSubquery
    }
  """
