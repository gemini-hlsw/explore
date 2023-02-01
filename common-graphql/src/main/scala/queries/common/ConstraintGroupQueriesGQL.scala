// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model
import lucuma.schemas.ObservationDB

import java.time

// gql: import io.circe.refined.*
// gql: import lucuma.schemas.decoders.*

object ConstraintGroupQueriesGQL {

  @GraphQL
  trait ConstraintGroupObsQuery extends GraphQLOperation[ObservationDB] {
    val document = """
      query($programId: ProgramId!) {
        constraintSetGroup(programId: $programId) {
          matches {
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
              matches {
                id
              }
            }
          }
        }

        observations(programId: $programId) {
          matches {
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
            observingMode {
              gmosNorthLongSlit {
                grating
                filter
                fpu
                centralWavelength {
                  picometers
                }
              }
              gmosSouthLongSlit {
                grating
                filter
                fpu
                centralWavelength {
                  picometers
                }
              }
            }
          }
        }
      }
      """

    object Data {
      object ConstraintSetGroup {
        type Matches = model.ConstraintGroup
      }

      object Observations {
        object Matches {
          object PlannedTime {
            type Execution = time.Duration
          }
          type ObservingMode = model.BasicConfiguration
        }

      }
    }
  }
}
