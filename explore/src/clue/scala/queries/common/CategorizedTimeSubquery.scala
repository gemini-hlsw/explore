// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import lucuma.core.model.sequence.CategorizedTime
import lucuma.schemas.ObservationDB
import lucuma.odb.json.timeaccounting.given

@GraphQL
object CategorizedTimeSubquery
    extends GraphQLSubquery.Typed[ObservationDB, CategorizedTime]("CategorizedTime"):
  override val subquery: String = """
    {
      program {
        microseconds
      }
      partner {
        microseconds
      }
      nonCharged {
        microseconds
      }
    }
  """
