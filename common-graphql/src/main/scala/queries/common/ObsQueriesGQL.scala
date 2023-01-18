// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model
import explore.model.ConstraintsSummary
import lucuma.core.{model => coreModel}
import lucuma.schemas.ObservationDB

import java.time
// gql: import explore.model.TargetWithId.*
// gql: import io.circe.refined.*
// gql: import lucuma.schemas.decoders.*

object ObsQueriesGQL {
  @GraphQL
  trait ProgramObservationsQuery extends GraphQLOperation[ObservationDB] {
    // TODO We should do a single observations query and extract the constraint sets and targets from it.
    val document = """
      query($programId: ProgramId!) {
        observations(programId: $programId) {
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
            observingMode {
              gmosNorthLongSlit {
                initialGrating
                initialFilter
                initialFpu
                grating
                filter
                fpu
                explicitXBin
                explicitYBin
                explicitAmpReadMode
                explicitAmpGain
                explicitRoi
                explicitWavelengthDithers {
                  picometers
                }
                explicitSpatialOffsets {
                  microarcseconds
                }
              }
              gmosSouthLongSlit {
                initialGrating
                initialFilter
                initialFpu
                grating
                filter
                fpu
                explicitXBin
                explicitYBin
                explicitAmpReadMode
                explicitAmpGain
                explicitRoi
                explicitWavelengthDithers {
                  picometers
                }
                explicitSpatialOffsets {
                  microarcseconds
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
            observations {
              matches {
                id
              }
            }
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
          type ObservingMode = model.ScienceMode
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
        observationEdit(input: {programId: $programId}) {
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
  trait ObsEditQuery extends GraphQLOperation[ObservationDB] {
    val document = """
      query($programId: ProgramId!, $obsId: ObservationId!) {
        observation(observationId: $obsId) {
          id
          title
          subtitle
          visualizationTime
          posAngleConstraint {
            mode
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
              capability
            }
          }
          observingMode {
            gmosNorthLongSlit {
              initialGrating
              initialFilter
              initialFpu
              grating
              filter
              fpu
              explicitXBin
              explicitYBin
              explicitAmpReadMode
              explicitAmpGain
              explicitRoi
              explicitWavelengthDithers {
                picometers
              }
              explicitSpatialOffsets {
                microarcseconds
              }
            }
            gmosSouthLongSlit {
              initialGrating
              initialFilter
              initialFpu
              grating
              filter
              fpu
              explicitXBin
              explicitYBin
              explicitAmpReadMode
              explicitAmpGain
              explicitRoi
              explicitWavelengthDithers {
                picometers
              }
              explicitSpatialOffsets {
                microarcseconds
              }
            }
          }
        }

        itc(programId: $programId, observationId: $obsId) {
          result {
            ... on ItcSuccess {
              exposureTime {
                microseconds
              }
              exposures
              signalToNoise
            }
            ... on ItcMissingParams {
              params
            }
            ... on ItcServiceError {
              message
            }
          }
        }
      }
    """
    // TODO: Consider ItcMissingParams and ItcServiceError

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

        type ObservingMode = model.ScienceMode
      }

      type Itc = explore.model.OdbItcResult
    }

  }

  @GraphQL
  trait ObservationEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription($obsId: ObservationId!) {
        observationEdit(input: {observationId: $obsId}) {
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

  @GraphQL
  trait CloneObservationMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation ($input: CloneObservationInput!){
        cloneObservation(input: $input) {
          newObservation {
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
      object CloneObservation {
        object NewObservation {
          trait ConstraintSet extends ConstraintsSummary
          object PlannedTime {
            type Execution = time.Duration
          }
        }
      }
    }
  }
}
