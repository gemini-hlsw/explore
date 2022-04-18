// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import io.circe.Decoder
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.sequence._
import lucuma.schemas.ObservationDB
import lucuma.schemas.decoders._

// gql: import lucuma.ui.reusability._

object GeneratedSequenceSQL {

  @GraphQL
  trait SequenceSteps extends GraphQLOperation[ObservationDB] {
    val document = """
      query($obsId: ObservationId!) {
        observation(observationId: $obsId) {
          execution {
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

      fragment stepTimeFields on StepTime {
          configChange {
            microseconds
          }
          exposure {
            microseconds
          }
          readout {
            microseconds
          }
          write {
            microseconds
          }
          total {
            microseconds
          }
      }

      fragment northSequenceFields on GmosNorthAtom {
        id
        steps {
          id
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

    implicit def offsetComponentDecoder[T]: Decoder[Offset.Component[T]] = Decoder.instance(c =>
      c.downField("microarcseconds")
        .as[Long]
        .map(Angle.signedMicroarcseconds.reverse.andThen(Offset.Component.angle[T].reverse).get)
    )

    implicit val offsetDecoder: Decoder[Offset] = Decoder.instance(c =>
      for {
        p <- c.downField("p").as[Offset.P]
        q <- c.downField("q").as[Offset.Q]
      } yield Offset(p, q)
    )

    object Data {
      object Observation {
        object Execution {
          type Config = FutureExecutionConfig
        }
      }
    }
  }
}
