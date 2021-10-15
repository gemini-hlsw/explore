// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model
import lucuma.schemas.ObservationDB

import java.time

// gql: import explore.model.decoders._
// gql: import explore.model.reusability._
// gql: import lucuma.ui.reusability._

object TargetListGroupQueriesGQL {

  @GraphQL
  trait TargetListGroupObsQuery extends GraphQLOperation[ObservationDB] {
    val document: String = """
      query {
        scienceTargetListGroup(programId: "p-2") {
          targetEnvironments {
            id
            observation {
              id
            }
          }
          commonTargetList {
            ids
            name
            tracking {
              ... on Sidereal {
                catalogId {
                  name
                  id
                }
                coordinates {
                  ra {
                    microarcseconds
                  }
                  dec {
                    microarcseconds
                  }
                }
                epoch
                properMotion {
                  ra {
                    microarcsecondsPerYear
                  }
                  dec {
                    microarcsecondsPerYear
                  }
                }
                radialVelocity {
                  centimetersPerSecond
                }
                parallax {
                  microarcseconds
                }
              }
            }
            magnitudes {
              value
              band
              system
            }
          }
        }

        observations(programId: "p-2") {
          nodes {
            id
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
            targets {
              id
            }
          }
        }
      }
    """

    object Data {
      type ScienceTargetListGroup = model.TargetListGroup
      object Observations {
        object Nodes {
          trait ConstraintSet extends model.ConstraintsSummary
          object PlannedTime {
            type Execution = time.Duration
          }
        }
      }
    }
  }

  @GraphQL
  trait ReplaceScienceTargetListMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: BulkReplaceTargetListInput!) {
        replaceScienceTargetList(input: $input) {
          observation { id }
        }
      }
    """
  }
}
