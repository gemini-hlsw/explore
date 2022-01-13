// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model
import lucuma.schemas.ObservationDB

// gql: import explore.model.reusability._
// gql: import explore.model.TargetWithId._
// gql: import lucuma.ui.reusability._

object TargetQueriesGQL {

  @GraphQL
  trait TargetNameQuery extends GraphQLOperation[ObservationDB] {
    // FIXME Change this to an actual name pattern query when it's available in the API
    val document = """
      query {
        targetGroup(programId: "p-2") {
          nodes {
            target {
              id
              name
              tracking {
                ... on Sidereal {
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
              sourceProfile {
                point {
                  bandNormalized {
                    sed {
                      stellarLibrary
                      coolStar
                      galaxy
                      planet
                      quasar
                      hiiRegion
                      planetaryNebula
                      powerLaw
                      blackBodyTempK
                      fluxDensities {
                        wavelength {
                          picometers
                        }
                        density
                      }
                    }
                    brightnesses {
                      band
                      value
                      units
                      error
                    }
                  }
                }
              }
              catalogInfo {
                name
                id
                objectType
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
    }
  }

  @GraphQL
  trait CreateTargetMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: CreateTargetInput!) {
        createTarget(input: $input) {
          id
        }
      }
    """
  }

  @GraphQL
  trait DeleteTargetMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($targetId: TargetId!) {
        deleteTarget(targetId: $targetId) {
          id
        }
      }
    """
  }

  @GraphQL
  trait UndeleteTargetMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($targetId: TargetId!) {
        undeleteTarget(targetId: $targetId) {
          id
        }
      }
    """
  }

  @GraphQL
  trait UpdateTargetMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: EditTargetInput!) {
        updateTarget(input: $input) {
          id
        }
      }
    """
  }

  @GraphQL
  trait ProgramTargetEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription {
        targetEdit(programId:"p-2") {
          id
        }
      }
    """
  }
}
