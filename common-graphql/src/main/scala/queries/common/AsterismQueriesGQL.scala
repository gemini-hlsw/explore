// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*

import java.time
// gql: import lucuma.schemas.decoders.given

object AsterismQueriesGQL {
  @GraphQL
  trait AsterismGroupObsQuery extends GraphQLOperation[ObservationDB] {
    val document: String = s"""
      query($$programId: ProgramId!) {
        targets(WHERE: {programId: { EQ: $$programId }}) {
          matches $TargetWithIdSubquery
        }

        observations(programId: $$programId) {
          matches $ObservationSummarySubquery
        }
      }
    """
  }

  @GraphQL
  trait UpdateAsterismsMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: UpdateAsterismsInput!) {
        updateAsterisms(input: $input) {
          observations {
            id
          }
        }
      }
    """
  }
}
