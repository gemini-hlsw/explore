// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model
import explore.model.ConstraintsSummary
import explore.schemas.ObservationDB
import java.time

// gql: import explore.model.decoders._
// gql: import explore.model.reusability._
// gql: import io.circe.refined._
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
            observationTarget {
              type: __typename
              ... on Target {
                targetId: id
                targetName: name
              }
              ... on Asterism {
                asterismId: id
                asterismName: name
              }
            }
            constraintSet {
              imageQuality
              cloudExtinction
              skyBackground
              waterVapor
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
        object Nodes {
          type ConstraintSet = model.ConstraintSet
        }
      }
      object Observations       {
        object Nodes {
          trait ConstraintSet extends ConstraintsSummary
          object PlannedTime {
            type Execution = time.Duration
          }
        }
      }
    }
  }
}
