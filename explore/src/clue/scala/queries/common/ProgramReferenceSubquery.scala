// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import lucuma.core.model.ProgramReference
import lucuma.schemas.ObservationDB

@GraphQL
object ProgramReferenceSubquery
    extends GraphQLSubquery.Typed[ObservationDB, ProgramReference]("ProgramReference"):
  override val subquery: String = s"""
    {
      label
    }
  """
