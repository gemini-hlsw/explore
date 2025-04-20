// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import explore.model.ProgramTime
import lucuma.schemas.ObservationDB

@GraphQL
object ProgramTimeSubquery
    extends GraphQLSubquery.Typed[ObservationDB, ProgramTime]("CategorizedTime"):
  override val subquery: String = """
    {
      program {
        microseconds
      }
    }
  """
