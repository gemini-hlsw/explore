// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*

// gql: import lucuma.odb.json.configurationrequest.query.given
// gql: import io.circe.refined.given

object ObsQueriesGQL:
  @GraphQL
  trait ProgramCreateObservation extends GraphQLOperation[ObservationDB]:
    val document = s"""
      mutation($$createObservation: CreateObservationInput!) {
        createObservation(input: $$createObservation) {
          observation $ObservationSubquery
          meta:observation {
            groupIndex
          }
        }
      }
    """

  @GraphQL
  trait SequenceOffsets extends GraphQLOperation[ObservationDB]:
    val document = s"""
      fragment stepData on GmosNorthStep {
        id
        telescopeConfig {
          offset $OffsetSubquery
        }
      }

      query($$obsId: ObservationId!) {
        observation(observationId: $$obsId) {
          execution {
            config {
              instrument
              gmosSouth {
                acquisition {
                  nextAtom {
                    steps {
                      ...stepData
                    }
                  }
                  possibleFuture {
                    steps {
                      ...stepData
                    }
                  }
                }
                science {
                  nextAtom {
                    steps {
                      ...stepData
                    }
                  }
                  possibleFuture {
                    steps {
                      ...stepData
                    }
                  }
                }
              }
              gmosNorth {
                acquisition {
                  nextAtom {
                    steps {
                      ...stepData
                    }
                  }
                  possibleFuture {
                    steps {
                      ...stepData
                    }
                  }
                }
                science {
                  nextAtom {
                    steps {
                      ...stepData
                    }
                  }
                  possibleFuture {
                    steps {
                      ...stepData
                    }
                  }
                }
              }
            }
          }
        }
      }
    """

    object Data:
      object Observation:
        object Execution:
          type Config = explore.model.ExecutionOffsets

  @GraphQL
  trait ObservationEditSubscription extends GraphQLOperation[ObservationDB]:
    // We need to include the `value {id}` to avoid a bug in grackle.
    val document = """
      subscription($input: ObservationEditInput!) {
        observationEdit(input: $input) {
          observationId
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
  trait UpdateObservationTimesMutation extends GraphQLOperation[ObservationDB]:
    val document = """
      mutation ($input: UpdateObservationsTimesInput!){
        updateObservationsTimes(input: $input) {
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
          newObservation $ObservationSubquery
        }
      }
    """

  @GraphQL
  trait CreateConfigurationRequestMutation extends GraphQLOperation[ObservationDB]:
    val document = s"""
      mutation($$input: CreateConfigurationRequestInput!) {
        createConfigurationRequest(input: $$input) 
          $ConfigurationRequestSubquery
      }
    """

  @GraphQL
  trait ProgramObservationsDelta extends GraphQLOperation[ObservationDB] {
    val document = s"""
      subscription($$input: ObservationEditInput!) {
        observationEdit(input: $$input) {
          observationId
          value $ObservationSubquery
          meta:value {
            groupId
            groupIndex
            existence
          }
          editType
        }
      }
    """
  }
