// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model
import lucuma.schemas.ObservationDB

import java.time

// gql: import explore.model.reusability._
// gql: import explore.model.TargetWithId._
// gql: import lucuma.schemas.decoders._
// gql: import lucuma.ui.reusability._

object TargetListGroupQueriesGQL {

  @GraphQL
  trait TargetListGroupObsQuery extends GraphQLOperation[ObservationDB] {
    val document: String = """
      query {
        asterismGroup(programId: "p-2") {
          nodes {
            observationIds
            asterism {
              id
            }
          }
        }

        targetGroup(programId: "p-2") {
          nodes {
            observationIds
            target {
              id
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
                error
              }
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
              asterism {
                id
              }
            }
          }
        }
      }
    """

    object Data {
      object TargetGroup {
        object Nodes {
          type Target = model.TargetWithId
        }
      }

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
      mutation($input: BulkEditTargetEnvironmentInput!) {
        updateTargetEnvironment(input: $input) {
          id
        }
      }
    """
  }

  @GraphQL
  trait UpdateAsterismMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: BulkEditAsterismInput!) {
        updateAsterism(input: $input) {
          id
        }
      }
    """
  }
}
