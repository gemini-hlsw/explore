// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.{model => coreModel}
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*

import java.time
// gql: import io.circe.refined.*
// gql: import lucuma.schemas.decoders.given

object ObsQueriesGQL:
  @GraphQL
  trait ProgramCreateObservation extends GraphQLOperation[ObservationDB]:
    val document = s"""
      mutation($$createObservation: CreateObservationInput!) {
        createObservation(input: $$createObservation) {
          observation $ObservationSummarySubquery
        }
      }
    """

  @GraphQL
  trait ObsEditQuery extends GraphQLOperation[ObservationDB]:
    val document = s"""
      fragment stepDataGN on GmosNorthStep {
        id
        stepConfig {
          ... on Science {
            offset $OffsetSubquery
          }
        }
      }

      fragment stepDataGS on GmosSouthStep {
        id
        stepConfig {
          ... on Science {
            offset $OffsetSubquery
          }
        }
      }

      query($$programId: ProgramId!, $$obsId: ObservationId!) {
        observation(observationId: $$obsId) {
          id
          title
          subtitle
          visualizationTime
          posAngleConstraint $PosAngleConstraintSubquery
          targetEnvironment {
            asterism {
              id
            }
          }
          constraintSet $ConstraintSetSubquery
          scienceRequirements {
            mode
            spectroscopy {
              wavelength $WavelengthSubquery
              resolution
              signalToNoise
              signalToNoiseAt $WavelengthSubquery
              wavelengthCoverage $WavelengthDeltaSubquery
              focalPlane
              focalPlaneAngle $AngleSubquery
              capability
            }
          }
          observingMode $ObservingModeSubquery
        }

        itc(programId: $$programId, observationId: $$obsId) {
          result {
            exposureTime {
              milliseconds
            }
            exposures
            signalToNoise
          }
        }

        sequence(programId: $$programId, observationId: $$obsId) {
          executionConfig {
            ... on GmosSouthExecutionConfig {
              acquisition {
                nextAtom {
                  steps {
                    ...stepDataGS
                  }
                }
                possibleFuture {
                  steps {
                    ...stepDataGS
                  }
                }
              }
              science {
                nextAtom {
                  steps {
                    ...stepDataGS
                  }
                }
                possibleFuture {
                  steps {
                    ...stepDataGS
                  }
                }
              }
            }
            ... on GmosNorthExecutionConfig {
              acquisition {
                nextAtom {
                  steps {
                    ...stepDataGN
                  }
                }
                possibleFuture {
                  steps {
                    ...stepDataGN
                  }
                }
              }
              science {
                nextAtom {
                  steps {
                    ...stepDataGN
                  }
                }
                possibleFuture {
                  steps {
                    ...stepDataGN
                  }
                }
              }
            }
          }
        }
      }
    """

    object Scalars:
      type SignalToNoise = lucuma.core.math.SignalToNoise

    object Data:
      object Itc:
        object Result:
          type ExposureTime = lucuma.core.util.TimeSpan
          type Exposures    = NonNegInt

      object Sequence:
        type ExecutionConfig = explore.model.ExecutionOffsets

  @GraphQL
  trait ObservationEditSubscription extends GraphQLOperation[ObservationDB]:
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

  @GraphQL
  trait UpdateObservationMutation extends GraphQLOperation[ObservationDB]:
    val document = """
      mutation ($input: UpdateObservationsInput!){
        updateObservations(input: $input) {
          observations {
            id
          }
        }
      }
    """

  @GraphQL
  trait CreateConfigurationMutation extends GraphQLOperation[ObservationDB]:
    val document = s"""
      mutation ($$input: UpdateObservationsInput!){
        updateObservations(input: $$input) {
          observations {
            observingMode $ObservingModeSubquery
          }
        }
      }
    """

  @GraphQL
  trait CloneObservationMutation extends GraphQLOperation[ObservationDB]:
    val document = s"""
      mutation ($$input: CloneObservationInput!){
        cloneObservation(input: $$input) {
          newObservation $ObservationSummarySubquery
        }
      }
    """

  @GraphQL
  trait ProgramObservationsDelta extends GraphQLOperation[ObservationDB] {
    val document = s"""
      subscription($$programId: ProgramId!) {
        observationEdit(input: {programId: $$programId}) {
          value $ObservationSummarySubquery
          meta:value {
            existence
          }
        }
      }
    """
  }
