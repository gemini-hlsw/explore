// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model
import lucuma.core.{model => coreModel}
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*

import java.time
// gql: import io.circe.refined.*
// gql: import lucuma.schemas.decoders.given

object ObsQueriesGQL {
  @GraphQL
  trait ProgramObservationsQuery extends GraphQLOperation[ObservationDB] {
    // TODO We should do a single observations query and extract the constraint sets and targets from it.
    val document = s"""
      query($$programId: ProgramId!) {
        observations(programId: $$programId) {
          matches {
            id
            title
            subtitle
            constraintSet $ConstraintsSummarySubquery
            status
            activeStatus
            visualizationTime
            plannedTime {
              execution $TimeSpanSubquery
            }
            observingMode {
              gmosNorthLongSlit {
                grating
                filter
                fpu
                centralWavelength $WavelengthSubquery
              }
              gmosSouthLongSlit {
                grating
                filter
                fpu
                centralWavelength $WavelengthSubquery
              }
            }
          }
        }

        constraintSetGroup(programId: $$programId) {
          matches {
            constraintSet $ConstraintSetSubquery
            observations {
              matches {
                id
              }
            }
          }
        }

        targetGroup(programId: $$programId) {
          matches {
            observations {
              matches {
                id
              }
            }
            target {
              id
              sidereal {
                ra $AngleSubquery
                dec $AngleSubquery
              }
            }
          }
        }
      }
    """

    object Data {
      object Observations {
        object Matches {
          type ObservingMode = model.BasicConfiguration
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
    // We need to include the `value {id}` to avoid a bug in grackle.
    val document = """
      subscription($programId: ProgramId!) {
        observationEdit(input: {programId: $programId}) {
          id
          value {
            id
          }
        }
      }
    """
  }

  @GraphQL
  trait ProgramCreateObservation extends GraphQLOperation[ObservationDB] {
    val document = s"""
      mutation($$createObservation: CreateObservationInput!) {
        createObservation(input: $$createObservation) {
          observation {
            id
            title
            subtitle
            constraintSet $ConstraintsSummarySubquery
            status
            activeStatus
            plannedTime {
              execution $TimeSpanSubquery
            }
          }
        }
      }
    """
  }

  @GraphQL
  trait ObsEditQuery extends GraphQLOperation[ObservationDB] {
    val document = s"""
      query($$programId: ProgramId!, $$obsId: ObservationId!) {
        observation(observationId: $$obsId) {
          id
          title
          subtitle
          visualizationTime
          posAngleConstraint $PosAngleConstraintSubquery
          targetEnvironment {
            asterism $TargetWithIdSubquery
          }
          constraintSet $ConstraintSetSubquery
          scienceRequirements {
            mode
            spectroscopy {
              wavelength $WavelengthSubquery
              resolution
              signalToNoise
              signalToNoiseAt $WavelengthSubquery
              wavelengthCoverage $WavelengthRangeSubquery
              focalPlane
              focalPlaneAngle $AngleSubquery
              capability
            }
          }
          observingMode {
            gmosNorthLongSlit {
              initialGrating
              initialFilter
              initialFpu
              initialCentralWavelength $WavelengthSubquery
              grating
              filter
              fpu
              centralWavelength $WavelengthSubquery
              defaultXBin
              explicitXBin
              defaultYBin
              explicitYBin
              defaultAmpReadMode
              explicitAmpReadMode
              defaultAmpGain
              explicitAmpGain
              defaultRoi
              explicitRoi
              defaultWavelengthDithers $WavelengthDitherSubquery
              explicitWavelengthDithers $WavelengthDitherSubquery
              defaultSpatialOffsets {
                microarcseconds
              }
              explicitSpatialOffsets {
                microarcseconds
              }
            }
            gmosSouthLongSlit {
              initialGrating
              initialFilter
              initialFpu
              initialCentralWavelength $WavelengthSubquery
              grating
              filter
              fpu
              centralWavelength $WavelengthSubquery
              defaultXBin
              explicitXBin
              defaultYBin
              explicitYBin
              defaultAmpReadMode
              explicitAmpReadMode
              defaultAmpGain
              explicitAmpGain
              defaultRoi
              explicitRoi
              defaultWavelengthDithers $WavelengthDitherSubquery
              explicitWavelengthDithers $WavelengthDitherSubquery
              defaultSpatialOffsets {
                microarcseconds
              }
              explicitSpatialOffsets {
                microarcseconds
              }
            }
          }
        }

        itc(programId: $$programId, observationId: $$obsId) {
          result {
            ... on ItcSuccess {
              exposureTime $TimeSpanSubquery
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

    object Data {
      object Observation {
        object ScienceRequirements {
          object Spectroscopy {
            type WavelengthCoverage = lucuma.core.math.WavelengthRange
          }
        }

        type ObservingMode = model.ScienceMode
      }

      type Itc = explore.model.OdbItcResult
    }

  }

  @GraphQL
  trait ObservationEditSubscription extends GraphQLOperation[ObservationDB] {
    // We need to include the `value {id}` to avoid a bug in grackle.
    val document = """
      subscription($obsId: ObservationId!) {
        observationEdit(input: {observationId: $obsId}) {
          id
          value {
            id
          }
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
  trait CreateConfigurationMutation extends GraphQLOperation[ObservationDB] {
    val document = s"""
      mutation ($$input: UpdateObservationsInput!){
        updateObservations(input: $$input) {
          observations {
            observingMode {
              gmosNorthLongSlit {
                initialGrating
                initialFilter
                initialFpu
                initialCentralWavelength {
                    picometers
                  }
                grating
                filter
                fpu
                centralWavelength {
                    picometers
                  }
                defaultXBin
                explicitXBin
                defaultYBin
                explicitYBin
                defaultAmpReadMode
                explicitAmpReadMode
                defaultAmpGain
                explicitAmpGain
                defaultRoi
                explicitRoi
                defaultWavelengthDithers {
                  picometers
                }
                explicitWavelengthDithers {
                  picometers
                }
                defaultSpatialOffsets {
                  microarcseconds
                }
                explicitSpatialOffsets {
                  microarcseconds
                }
              }
              gmosSouthLongSlit {
                initialGrating
                initialFilter
                initialFpu
                initialCentralWavelength {
                    picometers
                  }
                grating
                filter
                fpu
                centralWavelength {
                  picometers
                }
                defaultXBin
                explicitXBin
                defaultYBin
                explicitYBin
                defaultAmpReadMode
                explicitAmpReadMode
                defaultAmpGain
                explicitAmpGain
                defaultRoi
                explicitRoi
                defaultWavelengthDithers {
                  picometers
                }
                explicitWavelengthDithers {
                  picometers
                }
                defaultSpatialOffsets {
                  microarcseconds
                }
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
      object UpdateObservations {
        object Observations {
          type ObservingMode = model.ScienceMode
        }
      }
    }
  }

  @GraphQL
  trait CloneObservationMutation extends GraphQLOperation[ObservationDB] {
    val document = s"""
      mutation ($$input: CloneObservationInput!){
        cloneObservation(input: $$input) {
          newObservation {
            id
            title
            subtitle
            constraintSet $ConstraintsSummarySubquery
            status
            activeStatus
            plannedTime {
              execution $TimeSpanSubquery
            }
          }
        }
      }
    """
  }
}
