// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import io.circe.Decoder
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.model.Visit

// gql: import lucuma.schemas.decoders.given

object VisitsSQL:

  @GraphQL
  trait Visits extends GraphQLOperation[ObservationDB]:
    val document = """
      query($obsId: ObservationId!) {
        observation(observationId: $obsId) {
          execution {
            executionConfig {
              instrument
              ...on GmosNorthExecutionConfig {
                visits {
                  id
                  created
                  startTime
                  endTime
                  duration {
                    microseconds
                  }
                  staticN:staticConfig {
                    stageMode
                    detector
                    mosPreImaging
                    nodAndShuffle {
                      ...nodAndShuffleFields
                    }                
                  }
                  stepsN:steps {
                    ...gmosNorthStepRecordFields
                  }
                  sequenceEvents {
                    id
                    received
                    payload {
                      command
                    }
                  }              
                }
              }
              ...on GmosSouthExecutionConfig {
                visits {
                  id
                  created
                  startTime
                  endTime
                  duration {
                    microseconds
                  }             
                  staticS:staticConfig {
                    stageMode
                    detector
                    mosPreImaging
                    nodAndShuffle {
                      ...nodAndShuffleFields
                    }
                  }
                  stepsS:steps {
                    ...gmosSouthStepRecordFields
                  }
                  sequenceEvents {
                    id
                    received
                    payload {
                      command
                    }
                  }
                }
              }          
            }
          }
        }
      }

      fragment nodAndShuffleFields on GmosNodAndShuffle {
        posA {
          p {
            microarcseconds
          }
          q {
            microarcseconds
          }
        }
        posB {
          p {
            microarcseconds
          }
          q {
            microarcseconds
          }
        }
        eOffset
        shuffleOffset
        shuffleCycles
      }

      fragment gmosNorthStepRecordFields on GmosNorthStepRecord {
        id
        created
        startTime
        endTime
        duration {
          microseconds
        }
        instrumentConfig {
          exposure {
            microseconds
          }
          readout {
            xBin
            yBin
            ampCount
            ampGain
            ampReadMode
          }
          dtax
          roi
          gratingConfig {
            grating
            order
            wavelength {
              picometers
            }
          }
          filter
          fpu {
            customMask {
              filename
              slitWidth
            }
            builtin
          }
        }
        stepConfig {
          stepType
          ...on Gcal {
            continuum
            arcs
            filter
            diffuser
            shutter
          }
          ...on Science {
            offset {
              p {
                microarcseconds
              }
              q {
                microarcseconds
              }
            }

          }
        }
        stepEvents {
          id
          received
          payload {
            sequenceType
            stepStage
          }
        }
        stepQaState
        datasets {
          id {
            index
          }
          filename
          qaState
        }
      }

      fragment gmosSouthStepRecordFields on GmosSouthStepRecord {
        id
        created
        startTime
        endTime
        duration {
          microseconds
        }
        instrumentConfig {
          exposure {
            microseconds
          }
          readout {
            xBin
            yBin
            ampCount
            ampGain
            ampReadMode
          }
          dtax
          roi
          gratingConfig {
            grating
            order
            wavelength {
              picometers
            }
          }
          filter
          fpu {
            customMask {
              filename
              slitWidth
            }
            builtin
          }
        }
        stepConfig {
          stepType
          ...on Gcal {
            continuum
            arcs
            filter
            diffuser
            shutter
          }
          ...on Science {
            offset {
              p {
                microarcseconds
              }
              q {
                microarcseconds
              }
            }

          }
        }
        stepEvents {
          id
          received
          payload {
            sequenceType
            stepStage
          }
        }
        stepQaState
        datasets {
          id {
            index
          }
          filename
          qaState
        }
      }
    """

    object Data:
      object Observation:
        object Execution:
          type ExecutionConfig = ExecutionVisits
