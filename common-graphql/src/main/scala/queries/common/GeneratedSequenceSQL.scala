// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.core.model.sequence.*
import lucuma.schemas.ObservationDB
import lucuma.schemas.decoders.given
import lucuma.schemas.odb.*

object GeneratedSequenceSQL:

  @GraphQL
  trait SequenceSteps extends GraphQLOperation[ObservationDB]:
    val document = s"""
      query($$programId: ProgramId!, $$obsId: ObservationId!) {
        sequence(programId: $$programId, observationId: $$obsId) {
          config:executionConfig {
            instrument
            ... on GmosNorthExecutionConfig {
              staticN: static {
                stageMode
                detector
                mosPreImaging
                nodAndShuffle {
                  ...nodAndShuffleFields
                }
              }
              acquisitionN:acquisition {
                nextAtom {
                  ...northSequenceFields
                }
                possibleFuture {
                  ...northSequenceFields
                }
              }
              scienceN:science {
                nextAtom {
                  ...northSequenceFields
                }
                possibleFuture {
                  ...northSequenceFields
                }
              }
            }
            ... on GmosSouthExecutionConfig {
              staticS: static {
                stageMode
                detector
                mosPreImaging
                nodAndShuffle {
                  ...nodAndShuffleFields
                }
              }
              acquisitionS: acquisition {
                nextAtom {
                  ...southSequenceFields
                }
                possibleFuture {
                  ...southSequenceFields
                }
              }
              scienceS:science {
                nextAtom {
                  ...southSequenceFields
                }
                possibleFuture {
                  ...southSequenceFields
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

      fragment stepTimeFields on StepTime {
          configChange $TimeSpanSubquery
          exposure $TimeSpanSubquery
          readout $TimeSpanSubquery
          write $TimeSpanSubquery
          total $TimeSpanSubquery
      }

      fragment northSequenceFields on GmosNorthAtom {
        id
        steps {
          id
          instrumentConfig {
            exposure $TimeSpanSubquery
            readout $GmosCcdModeSubquery
            dtax
            roi
            gratingConfig {
              grating
              order
              wavelength $WavelengthSubquery
            }
            filter
            fpu {
              builtin
            }
          }
          stepConfig {
            stepType
            ... on Gcal {
              continuum
              arcs
              filter
              diffuser
              shutter
            }
            ... on Science {
              offset $OffsetSubquery
            }
          }
          time {
            ...stepTimeFields
          }
          breakpoint
        }
        time {
          ...stepTimeFields
        }
      }

      fragment southSequenceFields on GmosSouthAtom {
        id
        steps {
          id
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
              wavelength $WavelengthSubquery
            }
            filter
            fpu {
              builtin
            }
          }
          stepConfig {
            stepType
            ... on Gcal {
              continuum
              arcs
              filter
              diffuser
              shutter
            }
            ... on Science {
              offset $OffsetSubquery
            }
          }
          time {
            ...stepTimeFields
          }
          breakpoint
        }
        time {
          ...stepTimeFields
        }
      }
    """

    object Data:
      object Sequence:
        type Config = FutureExecutionConfig
