// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model
import explore.model.ConstraintsSummary
import lucuma.core.math.Angle
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
    val document = """
      query {
        observations(programId: "p-2") {
          nodes {
            id
            targets {
              asterism {
                id
                name
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
        }
    """

    object Data {
      object Observations {
        object Nodes {
          trait ConstraintSet extends ConstraintsSummary
          object PlannedTime {
            type Execution = time.Duration
          }
        }
      }

      object ConstraintSetGroup {
        type Nodes = model.ConstraintGroup
      }
    }

  }

  @GraphQL
  trait ProgramObservationsEditSubscription extends GraphQLOperation[ObservationDB] {
    val document = """
      subscription {
        observationEdit(programId:"p-2") {
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
          id
        }
      }
    """
  }

  @GraphQL
  trait ProgramDeleteObservation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($oid: ObservationId!) {
        deleteObservation(observationId: $oid) {
          id
        }
      }
    """
  }

  @GraphQL
  trait ProgramUndeleteObservation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation($oid: ObservationId!) {
        undeleteObservation(observationId: $oid) {
          id
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
          targets {
            asterism {
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
          scienceRequirements {
            mode
            spectroscopyRequirements {
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
          scienceConfiguration {
            ... on GmosNorthLongSlit {
              filterN:filter
              disperserN:disperser
              slitWidthN:slitWidth {
                microarcseconds
              }
            }
            ... on GmosSouthLongSlit {
              filterS:filter
              disperserS:disperser
              slitWidthS:slitWidth {
                microarcseconds
              }
            }
          }
        }
      }
    """

    object Data {
      object Observation {
        object Targets {
          type Asterism = model.TargetWithId
        }
        type ConstraintSet = model.ConstraintSet

        object ScienceRequirements {
          object SpectroscopyRequirements {
            type Wavelength         = lucuma.core.math.Wavelength
            type SignalToNoiseAt    = lucuma.core.math.Wavelength
            type WavelengthCoverage = lucuma.core.math.Wavelength
            type FocalPlaneAngle    = lucuma.core.math.Angle
          }
        }

        object ScienceConfiguration {
          object GmosNorthLongSlit {
            type SlitWidthN = Angle
          }
          object GmosSouthLongSlit {
            type SlitWidthS = Angle
          }
        }
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
      mutation ($input: EditObservationInput!){
        updateObservation(input: $input) {
          id
        }
      }
    """
  }

  @GraphQL
  trait UpdateConstraintSetMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation ($obsIds: [ObservationId!]!, $input: EditConstraintSetInput!){
        updateConstraintSet(input: {selectObservations: $obsIds, edit: $input}) {
          id
        }
      }
    """
  }

  @GraphQL
  trait UpdateScienceRequirementsMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation ($obsIds: ObservationId!, $input: EditScienceRequirementsInput!){
        updateScienceRequirements(input: {selectObservations: [$obsIds], edit: $input}) {
          id
        }
      }
    """
  }

  @GraphQL
  trait UpdateScienceConfigurationMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation ($obsId: ObservationId!, $input: CreateObservationConfigInput){
        updateObservation(input: {observationId: $obsId, scienceConfiguration: {set: $input}}) {
          id
        }
      }
    """
  }

}
