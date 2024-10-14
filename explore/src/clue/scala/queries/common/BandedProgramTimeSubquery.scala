// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import explore.model.BandedProgramTime
import lucuma.schemas.ObservationDB

@GraphQL
object BandedProgramTimeSubquery
    extends GraphQLSubquery.Typed[ObservationDB, BandedProgramTime]("BandedTime"):
  override val subquery: String = s"""
        {
          band
          time $ProgramTimeSubquery
        }
      """
