// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model
import lucuma.schemas.ObservationDB

import java.time

// gql: import lucuma.schemas.decoders.*

object AsterismQueriesGQL {

  @GraphQL
  trait AsterismGroupObsQuery extends GraphQLOperation[ObservationDB] {
    val document: String = """
      query($programId: ProgramId!) {
        asterismGroup(programId: $programId) {
          matches {
            observations {
              matches {
                id
              }
            }
            asterism {
              id
            }
          }
        }

        targetGroup(programId: $programId) {
          matches {
            observations {
              matches {
                id
              }
            }
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

        observations(programId: $programId) {
          matches {
            id
            constraintSet {
              imageQuality
              cloudExtinction
              skyBackground
              waterVapor
            }
            status
            activeStatus
            visualizationTime
            posAngleConstraint {
              mode
              angle {
                microarcseconds
              }
            }
            plannedTime {
              execution {
                microseconds
              }
            }
            targetEnvironment {
              asterism {
                id
              }
            }
            scienceRequirements {
              spectroscopy {
                wavelength {
                  picometers
                }
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
      object TargetGroup {
        type Matches = explore.model.TargetWithIdAndObs
      }

      object Observations {
        object Matches {
          type PosAngleConstraint = lucuma.core.model.PosAngleConstraint
          trait ConstraintSet extends model.ConstraintsSummary
          type ObservingMode = model.BasicConfiguration
          object PlannedTime {
            type Execution = time.Duration
          }

          object ScienceRequirements {
            object Spectroscopy {
              type Wavelength = lucuma.core.math.Wavelength
            }
          }
        }
      }
    }
  }

  @GraphQL
  trait UpdateAsterismsMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: UpdateAsterismsInput!) {
        updateAsterisms(input: $input) {
          observations {
            id
          }
        }
      }
    """
  }
}
