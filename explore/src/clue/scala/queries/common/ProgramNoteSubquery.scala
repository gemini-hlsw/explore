// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLSubquery
import clue.annotation.GraphQL
import explore.model.ProgramNote
import lucuma.schemas.ObservationDB

@GraphQL
object ProgramNoteSubquery
    extends GraphQLSubquery.Typed[ObservationDB, ProgramNote]("ProgramNote") {
  override val subquery: String = """
    {
      id
      title
      text
      isPrivate
    }
  """
}
