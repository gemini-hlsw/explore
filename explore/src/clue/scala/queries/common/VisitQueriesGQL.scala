// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*
// gql: import lucuma.schemas.decoders.given

object VisitQueriesGQL:

  @GraphQL
  trait ObservationVisits extends GraphQLOperation[ObservationDB]:
    val document = s"""
      query($$obsId: ObservationId!, $$visitIdOffset: VisitId) {
        observation(observationId: $$obsId) {
          execution $ExecutionVisitsSubquery
        }
      }

      fragment nodAndShuffleFields on GmosNodAndShuffle {
        posA $OffsetSubquery
        posB $OffsetSubquery
        eOffset
        shuffleOffset
        shuffleCycles
      }
    """
    // ${ExecutionVisitsSubquery.Fragments}
