// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import lucuma.core.model.ObservationValidation
import lucuma.schemas.ObservationDB

@GraphQL
object ObservationValidationsSubquery
    extends GraphQLSubquery.Typed[ObservationDB, ObservationValidation]("ObservationValidation"):

  override val subquery: String = s"""
    {
      code
      messages
    }
  """
