// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model
import explore.model.ConstraintsSummary
import explore.schemas.ObservationDB
import io.circe.Decoder
import lucuma.core.math.Angle

import java.time
// gql: import explore.model.decoders._
// gql: import explore.model.reusability._
// gql: import io.circe.refined._
// gql: import lucuma.ui.reusability._

object ObsQueriesGQL {
  implicit val posAngleDecoder: Decoder[Angle] = Decoder.instance(
    _.downField("microarcseconds").as[Long].map(Angle.microarcseconds.reverseGet)
  )

  @GraphQL
  trait ProgramObservationsQuery extends GraphQLOperation[ObservationDB] {
    val document = """
      query {
        observations(programId: "p-2") {
          nodes {
            id
            observationTarget {
              type: __typename
              ... on Target {
                targetId: id
                targetName: name
              }
              ... on Asterism {
                asterismId: id
                asterismName: name
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
          constraintSet {
            name
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
              wavelengthRange {
                picometers
              }
              focalPlane
              focalPlaneAngle {
                microarcseconds
              }
              capabilities
            }
          }
        }
      }
    """

    object Data {
      object Observation {
        object ConstraintSet {
          type ElevationRange = model.ElevationRange
        }

        object ScienceRequirements {
          object SpectroscopyRequirements {
            type Wavelength      = lucuma.core.math.Wavelength
            type SignalToNoiseAt = lucuma.core.math.Wavelength
            type WavelengthRange = lucuma.core.math.Wavelength
            type FocalPlaneAngle = lucuma.core.math.Angle
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
      mutation ($obsId: ObservationId!, $input: EditConstraintSetInput!){
        updateConstraintSet(input: {observationIds: [$obsId], constraintSet: $input}) {
          id
        }
      }
    """
  }

  @GraphQL
  trait UpdateScienceRequirementsMutation extends GraphQLOperation[ObservationDB] {
    val document = """
      mutation ($obsId: ObservationId!, $input: EditScienceRequirementsInput!){
        updateScienceRequirements(input: {observationIds: [$obsId], scienceRequirements: $input}) {
          id
        }
      }
    """
  }

}
