// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model
import explore.model.ConstraintsSummary
import lucuma.core.{model => coreModel}
import lucuma.schemas.ObservationDB

import java.time
// gql: import explore.model.reusability._
// gql: import explore.model.TargetWithId._
// gql: import io.circe.refined._
// gql: import lucuma.schemas.decoders._
// gql: import lucuma.ui.reusability._

object ObsQueriesGQL {
  @GraphQL
  trait ProgramObservationsQuery extends GraphQLOperation[ObservationDB] {
    // TODO We should do a single observations query and extract the constraint sets and targets from it.
    val document = """
      query($programId: ProgramId!) {
        observations(WHERE: {programId: {EQ: $programId}}) {
          matches {
            id
            title
            subtitle
            constraintSet {
              imageQuality
              cloudExtinction
              skyBackground
              waterVapor
            }
            status
            activeStatus
            visualizationTime
            plannedTime {
              execution {
                microseconds
              }
            }
            scienceMode {
              gmosNorthLongSlit {
                basic {
                  grating
                  filter
                  fpu
                }
                advanced {
                  overrideGrating
                  overrideFilter
                  overrideFpu
                  overrideExposureTimeMode {
                    signalToNoise {
                      value
                    }
                    fixedExposure {
                      count
                      time {
                        microseconds
                      }
                    }
                  }
                  explicitXBin
                  explicitYBin
                  explicitAmpReadMode
                  explicitAmpGain
                  explicitRoi
                  explicitWavelengthDithersNm
                  explicitSpatialOffsets {
                    microarcseconds
                  }
                }
              }
              gmosSouthLongSlit {
                basic {
                  grating
                  filter
                  fpu
                }
                advanced {
                  overrideGrating
                  overrideFilter
                  overrideFpu
                  overrideExposureTimeMode {
                    signalToNoise {
                      value
                    }
                    fixedExposure {
                      count
                      time {
                        microseconds
                      }
                    }
                  }
                  explicitXBin
                  explicitYBin
                  explicitAmpReadMode
                  explicitAmpGain
                  explicitRoi
                  explicitWavelengthDithersNm
                  explicitSpatialOffsets {
                    microarcseconds
                  }
                }
              }
            }
          }
        }

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

        targetGroup(programId: $programId) {
          matches {
            observationIds
            target {
              id
              sidereal {
                ra {
                  microarcseconds
                }
                dec {
                  microarcseconds
                }
              }
            }
          }
        }
      }
    """

    object Data {
      object Observations {
        object Matches {
          trait ConstraintSet extends ConstraintsSummary
          object PlannedTime {
            type Execution = time.Duration
          }
          type ScienceMode = model.ScienceMode
        }
      }

      object ConstraintSetGroup {
        type Matches = model.ConstraintGroup
      }

      object TargetGroup {
        object Matches {
          object Target {
            type Sidereal = lucuma.core.math.Coordinates
          }
        }
      }
    }

  }

  @GraphQL
  trait ProgramObservationsEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription($programId: ProgramId!) {
        observationEdit(programId: $programId) {
          id
        }
      }
    """
  }

  @GraphQL
  trait ProgramCreateObservation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($createObservation: CreateObservationInput!) {
        createObservation(input: $createObservation) {
          observation {
            id
            title
            subtitle
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
      object CreateObservation {
        object Observation {
          trait ConstraintSet extends ConstraintsSummary
          object PlannedTime {
            type Execution = time.Duration
          }
        }
      }
    }
  }

  @GraphQL
  trait ProgramDeleteObservations extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: DeleteObservationsInput!) {
        deleteObservations(input: $input) {
          observations {
            id
          }
        }
      }
    """
  }

  @GraphQL
  trait ProgramUndeleteObservations extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($input: UndeleteObservationsInput!) {
        undeleteObservations(input: $input) {
          observations {
            id
          }
        }
      }
    """
  }

  @GraphQL
  trait ObsEditQuery extends GraphQLOperation[ObservationDB] {
    val document = """
      query($obsId: ObservationId!) {
        observation(observationId: $obsId) {
          id
          title
          subtitle
          visualizationTime
          posAngleConstraint {
            constraint
            angle {
              microarcseconds
            }
          }
          targetEnvironment {
            asterism {
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
          scienceRequirements {
            mode
            spectroscopy {
              wavelength {
                picometers
              }
              resolution
              signalToNoise
              signalToNoiseAt {
                picometers
              }
              wavelengthCoverage {
                picometers
              }
              focalPlane
              focalPlaneAngle {
                microarcseconds
              }
              capabilities
            }
          }
          scienceMode {
            gmosNorthLongSlit {
              basic {
                grating
                filter
                fpu
              }
              advanced {
                overrideGrating
                overrideFilter
                overrideFpu
                overrideExposureTimeMode {
                  signalToNoise {
                    value
                  }
                  fixedExposure {
                    count
                    time {
                      microseconds
                    }
                  }
                }
                explicitXBin
                explicitYBin
                explicitAmpReadMode
                explicitAmpGain
                explicitRoi
                explicitWavelengthDithersNm
                explicitSpatialOffsets {
                  microarcseconds
                }
              }
            }
            gmosSouthLongSlit {
              basic {
                grating
                filter
                fpu
              }
              advanced {
                overrideGrating
                overrideFilter
                overrideFpu
                overrideExposureTimeMode {
                  signalToNoise {
                    value
                  }
                  fixedExposure {
                    count
                    time {
                      microseconds
                    }
                  }
                }
                explicitXBin
                explicitYBin
                explicitAmpReadMode
                explicitAmpGain
                explicitRoi
                explicitWavelengthDithersNm
                explicitSpatialOffsets {
                  microarcseconds
                }
              }
            }
          }
        }
      }
    """

    object Data {
      object Observation {
        type PosAngleConstraint = lucuma.core.model.PosAngleConstraint

        object TargetEnvironment {
          type Asterism = model.TargetWithId
        }
        type ConstraintSet = coreModel.ConstraintSet

        object ScienceRequirements {
          object Spectroscopy {
            type Wavelength         = lucuma.core.math.Wavelength
            type SignalToNoiseAt    = lucuma.core.math.Wavelength
            type WavelengthCoverage = lucuma.core.math.Wavelength
            type FocalPlaneAngle    = lucuma.core.math.Angle
          }
        }

        type ScienceMode = model.ScienceMode
      }
    }

  }

  @GraphQL
  trait ObservationEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription($obsId: ObservationId!) {
        observationEdit(observationId: $obsId) {
          id
        }
      }
    """
  }

  @GraphQL
  trait UpdateObservationMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation ($input: UpdateObservationsInput!){
        updateObservations(input: $input) {
          observations {
            id
          }
        }
      }
    """
  }
}
