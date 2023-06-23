// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.core.model.sequence.*
import lucuma.schemas.ObservationDB
import lucuma.schemas.odb.*
// gql: import lucuma.odb.json.sequence.given

object GeneratedSequenceSQL:

  @GraphQL
  trait SequenceSteps extends GraphQLOperation[ObservationDB]:
    val document = s"""
      query($$programId: ProgramId!, $$obsId: ObservationId!) {
        sequence(programId: $$programId, observationId: $$obsId) {
          executionConfig {
            instrument
            ... on GmosNorthExecutionConfig {
              gmosNorth
              static {
                stageMode
                detector
                mosPreImaging
                nodAndShuffle {
                  ...nodAndShuffleFields
                }
              }
              acquisition {
                ...gmosNorthSequenceFields
              }
              science {
                ...gmosNorthSequenceFields
              }
              setup {
                ...setupTimeFields
              }
            }
            ... on GmosSouthExecutionConfig {
              gmosSouth
              static {
                stageMode
                detector
                mosPreImaging
                nodAndShuffle {
                  ...nodAndShuffleFields
                }
              }
              acquisition {
                ...gmosSouthSequenceFields
              }
              science {
                ...gmosSouthSequenceFields
              }
              setup {
                ...setupTimeFields
              }
            }
          }
        }
      }

      fragment setupTimeFields on SetupTime {
        full { microseconds }
        reacquisition { microseconds }
      }

      fragment nodAndShuffleFields on GmosNodAndShuffle {
        posA $OffsetSubquery
        posB $OffsetSubquery
        eOffset
        shuffleOffset
        shuffleCycles
      }

      fragment sequenceDigestFields on SequenceDigest {
        observeClass
        plannedTime {
          charges {
            chargeClass
            time { microseconds }
          }
        }
        offsets {
          p { microarcseconds }
          q { microarcseconds }
        }
      }

      fragment stepConfigFields on StepConfig {
        stepType
        ... on Gcal {
          continuum
          arcs
          filter
          diffuser
          shutter
        }
        ... on Science {
          offset {
            p { microarcseconds }
            q { microarcseconds }
          }
          guiding
        }
        ... on SmartGcal {
          smartGcalType
        }
      }

      fragment stepEstimateFields on StepEstimate {
        configChange {
          all {
            name
            description
            estimate { microseconds }
          }
          index
        }
        detector {
          all {
            name
            description
            dataset {
              exposure { microseconds }
              readout { microseconds }
              write { microseconds }
            }
            count
          }
          index
        }
      }

      fragment gmosNorthAtomFields on GmosNorthAtom {
        id
        description
        steps {
          id
          instrumentConfig {
            exposure { microseconds }
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
              wavelength { picometers }
            }
            filter
            fpu {
              builtin
            }
          }
          stepConfig {
            ...stepConfigFields
          }
          estimate {
            ...stepEstimateFields
          }
          observeClass
          breakpoint
        }
      }

      fragment gmosNorthSequenceFields on GmosNorthExecutionSequence {
        digest {
          ...sequenceDigestFields
        }
        nextAtom {
          ...gmosNorthAtomFields
        }
        possibleFuture {
          ...gmosNorthAtomFields
        }
        hasMore
        atomCount
      }

      fragment gmosSouthAtomFields on GmosSouthAtom {
        id
        description
        steps {
          id
          instrumentConfig {
            exposure { microseconds }
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
              wavelength { picometers }
            }
            filter
            fpu {
              builtin
            }
          }
          stepConfig {
            ...stepConfigFields
          }
          estimate {
            ...stepEstimateFields
          }
          observeClass
          breakpoint
        }
      }

      fragment gmosSouthSequenceFields on GmosSouthExecutionSequence {
        digest {
          ...sequenceDigestFields
        }
        nextAtom {
          ...gmosSouthAtomFields
        }
        possibleFuture {
          ...gmosSouthAtomFields
        }
        hasMore
        atomCount
      }
    """

    object Data:
      object Sequence:
        type ExecutionConfig = InstrumentExecutionConfig
