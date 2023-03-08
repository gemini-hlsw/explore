// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*

import java.time
// gql: import io.circe.refined.*
// gql: import lucuma.schemas.decoders.given

object ConstraintGroupQueriesGQL {

  @GraphQL
  trait ConstraintGroupObsQuery extends GraphQLOperation[ObservationDB] {
    val document = s"""
      query($$programId: ProgramId!) {
        constraintSetGroup(programId: $$programId) {
          matches {
            constraintSet $ConstraintSetSubquery
            observations {
              matches {
                id
              }
            }
          }
        }

        observations(programId: $$programId) {
          matches {
            id
            title
            subtitle
            status
            activeStatus
            plannedTime {
              execution $TimeSpanSubquery
            }
            observingMode $BasicConfigurationSubquery
          }
        }
      }
      """

    object Data {
      object ConstraintSetGroup {
        type Matches = model.ConstraintGroup
      }
    }
  }
}
