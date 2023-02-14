// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import io.circe.Decoder
import lucuma.schemas.ObservationDB
import lucuma.schemas.model.ExecutionVisits
import lucuma.schemas.model.Visit
import lucuma.schemas.odb.*
// gql: import lucuma.schemas.decoders.given

object VisitsSQL:

  @GraphQL
  trait Visits extends GraphQLOperation[ObservationDB]:
    val document = s"""
      query($$obsId: ObservationId!) {
        observation(observationId: $$obsId) {
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
                  duration $TimeSpanSubquery         
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
        posA $OffsetSubquery
        posB $OffsetSubquery
        eOffset
        shuffleOffset
        shuffleCycles
      }

      fragment gmosNorthStepRecordFields on GmosNorthStepRecord {
        id
        created
        startTime
        endTime
        duration $TimeSpanSubquery
        instrumentConfig {
          exposure {
            microseconds
          }
          readout $GmosCcdModeSubquery
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
            offset $OffsetSubquery
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
        duration $TimeSpanSubquery
        instrumentConfig {
          exposure {
            microseconds
          }
          readout $GmosCcdModeSubquery
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
            offset $OffsetSubquery
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
