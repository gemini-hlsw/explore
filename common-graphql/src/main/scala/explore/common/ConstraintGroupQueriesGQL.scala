// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

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
      query {
        constraintSetGroup(programId: "p-2") {
          nodes {
            constraintSet {
              cloudExtinction
              imageQuality
              skyBackground
              waterVapor
              elevationRange {
                type: __typename
                ... on AirMassRange {
                  min
                  max
                }
                ... on HourAngleRange {
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

        observations(programId: "p-2") {
          nodes {
            id
            targetEnvironment {
              asterism {
                id
                name
              }
            }
          	status
          	activeStatus
          	plannedTime {
          	  execution {
          	    microseconds
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
      object Observations       {
        object Nodes {
          object PlannedTime {
            type Execution = time.Duration
          }
        }
      }
    }
  }
}
