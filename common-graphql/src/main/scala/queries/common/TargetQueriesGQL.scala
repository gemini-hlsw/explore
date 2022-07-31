// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model
import lucuma.schemas.ObservationDB

// gql: import explore.model.TargetWithId._

object TargetQueriesGQL {

  @GraphQL
  trait TargetNameQuery extends GraphQLOperation[ObservationDB] {
    // FIXME Change this to an actual name pattern query when it's available in the API
    val document = """
      query($programId: ProgramId!) {
        targetGroup(programId: $programId) {
          matches {
            target {
              id
              name
              sidereal {
                ra {
                  microarcseconds
                }
                dec {
                  microarcseconds
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
                catalogInfo {
                  name
                  id
                  objectType
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
                  emissionLines {
                    lines {
                      wavelength {
                        picometers
                      }
                      lineWidth
                      lineFlux {
                        value
                        units
                      }
                    }
                    fluxDensityContinuum {
                      value
                      units
                    }
                  }
                }
                uniform {
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
                  emissionLines {
                    lines {
                      wavelength {
                        picometers
                      }
                      lineWidth
                      lineFlux {
                        value
                        units
                      }
                    }
                    fluxDensityContinuum {
                      value
                      units
                    }
                  }
                }
                gaussian {
                  fwhm {
                    microarcseconds
                  }
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
                  emissionLines {
                    lines {
                      wavelength {
                        picometers
                      }
                      lineWidth
                      lineFlux {
                        value
                        units
                      }
                    }
                    fluxDensityContinuum {
                      value
                      units
                    }
                  }
                }
              }
            }
          }
        }
      }
    """

    object Data {
      object TargetGroup {
        object Matches {
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
          target {
            id
          }
        }
      }
    """
  }

  @GraphQL
  trait DeleteTargetsMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: DeleteTargetsInput!) {
        deleteTargets(input: $input) {
          targets {
            id
          }
        }
      }
    """
  }

  @GraphQL
  trait UndeleteTargetsMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: UndeleteTargetsInput!) {
        undeleteTargets(input: $input) {
          targets {
            id
          }
        }
      }
    """
  }

  @GraphQL
  trait UpdateTargetsMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: UpdateTargetsInput!) {
        updateTargets(input: $input) {
          targets {
            id
          }
        }
      }
    """
  }

  @GraphQL
  trait UpdateTargetsMutationWithResult extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: UpdateTargetsInput!) {
        updateTargets(input: $input) {
          targets {
            id
            name
            sidereal {
              ra {
                microarcseconds
              }
              dec {
                microarcseconds
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
              catalogInfo {
                name
                id
                objectType
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
                emissionLines {
                  lines {
                    wavelength {
                      picometers
                    }
                    lineWidth
                    lineFlux {
                      value
                      units
                    }
                  }
                  fluxDensityContinuum {
                    value
                    units
                  }
                }
              }
              uniform {
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
                emissionLines {
                  lines {
                    wavelength {
                      picometers
                    }
                    lineWidth
                    lineFlux {
                      value
                      units
                    }
                  }
                  fluxDensityContinuum {
                    value
                    units
                  }
                }
              }
              gaussian {
                fwhm {
                  microarcseconds
                }
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
                emissionLines {
                  lines {
                    wavelength {
                      picometers
                    }
                    lineWidth
                    lineFlux {
                      value
                      units
                    }
                  }
                  fluxDensityContinuum {
                    value
                    units
                  }
                }
              }
            }
          }
        }
      }
    """

    object Data {
      object UpdateTargets {
        type Targets = model.TargetWithId
      }
    }
  }

  @GraphQL
  trait CloneTargetMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: CloneTargetInput!) {
        cloneTarget(input: $input) {
          newTarget {
            id
          }
        }
      }
    """
  }

  @GraphQL
  trait ProgramTargetEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription($programId: ProgramId!) {
        targetEdit(programId: $programId) {
          id
        }
      }
    """
  }
}
