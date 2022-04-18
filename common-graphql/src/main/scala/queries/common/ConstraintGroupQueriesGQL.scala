// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model
import lucuma.schemas.ObservationDB

import java.time

// gql: import explore.model.reusability._
// gql: import io.circe.refined._
// gql: import lucuma.schemas.decoders._
// gql: import lucuma.ui.reusability._

object ConstraintGroupQueriesGQL {

  @GraphQL
  trait ConstraintGroupObsQuery extends GraphQLOperation[ObservationDB] {
    val document = """
      query($programId: ProgramId!) {
        constraintSetGroup(programId: $programId) {
          nodes {
            constraintSet {
              cloudExtinction
              imageQuality
              skyBackground
              waterVapor
              elevationRange {
                airMass {
                  min
                  max
                }
                hourAngle {
                  minHours
                  maxHours
                }
              }
            }
            observations {
              nodes {
                id
              }
            }
          }
        }

        observations(programId: $programId) {
          nodes {
            id
            title
            subtitle
          	status
          	activeStatus
          	plannedTime {
          	  execution {
          	    microseconds
          	  }
          	}
            scienceConfiguration {
              gmosNorthLongSlit {
                filter
                disperser
                fpu
                slitWidth {
                  microarcseconds
                }
              }
              gmosSouthLongSlit {
                filter
                disperser
                fpu
                slitWidth {
                  microarcseconds
                }
              }
            }
          }
        }
      }
      """

    object Data {
      object ConstraintSetGroup {
        type Nodes = model.ConstraintGroup
      }

      object Observations {
        object Nodes {
          object PlannedTime {
            type Execution = time.Duration
          }
          type ScienceConfiguration = model.ScienceConfiguration
        }

      }
    }
  }
}
