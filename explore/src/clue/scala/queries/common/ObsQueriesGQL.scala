// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.model
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*

// gql: import io.circe.refined.*
// gql: import lucuma.odb.json.time.decoder.given

object ObsQueriesGQL:
  @GraphQL
  trait ProgramCreateObservation extends GraphQLOperation[ObservationDB]:
    val document = s"""
      mutation($$createObservation: CreateObservationInput!) {
        createObservation(input: $$createObservation) {
          observation $ObservationSubquery
        }
      }
    """

  @GraphQL
  trait ObsItcQuery extends GraphQLOperation[ObservationDB]:
    val document = s"""
      query($$obsId: ObservationId!) {
        observation(observationId: $$obsId) {
          itc {
            science {
              selected {
                exposureTime $TimeSpanSubquery
                exposures
                signalToNoise
              }
            }
            acquisition {
              selected {
                exposures
                signalToNoise
              }
            }
          }
        }
      }
    """

  @GraphQL
  trait SequenceOffsets extends GraphQLOperation[ObservationDB]:
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

      query($$obsId: ObservationId!) {
        observation(observationId: $$obsId) {
          execution {
            config {
              instrument
              gmosSouth {
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
              gmosNorth {
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
          newObservation $ObservationSubquery
        }
      }
    """

  @GraphQL
  trait ProgramObservationsDelta extends GraphQLOperation[ObservationDB] {
    val document = s"""
      subscription($$input: ObservationEditInput!) {
        observationEdit(input: $$input) {
          value $ObservationSubquery
          meta:value {
            existence
          }
          editType
        }
      }
    """
  }
