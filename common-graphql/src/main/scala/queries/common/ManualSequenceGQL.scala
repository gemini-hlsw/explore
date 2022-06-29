// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import io.circe.Decoder
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.sequence.ManualConfig
import lucuma.schemas.ObservationDB

// gql: import lucuma.schemas.decoders._
// gql: import lucuma.ui.reusability._

object ManualSequenceGQL {

  @GraphQL
  trait SequenceSteps extends GraphQLOperation[ObservationDB] {
    val document = """
      query($whereObservation: WhereObservation!) {
        observations(WHERE: $whereObservation, LIMIT: 1) {
          matches {
            config:manualConfig {
              instrument
              plannedTime {
                setup {
                  microseconds
                }
              }
              ... on GmosNorthManualConfig {
                staticN: static {
                  stageMode
                  detector
                  mosPreImaging
                  nodAndShuffle {
                    ...nodAndShuffleFields
                  }
                }
                acquisitionN: acquisition {
                  atoms {
                    ...northSequenceFields
                  }
                  time {
                    ...stepTimeFields
                  }
                }
                scienceN: science {
                  atoms {
                    ...northSequenceFields
                  }
                  time {
                    ...stepTimeFields
                  }
                }
              }
              ... on GmosSouthManualConfig {
                staticS: static {
                  stageMode
                  detector
                  mosPreImaging
                  nodAndShuffle {
                    ...nodAndShuffleFields
                  }
                }
                acquisitionS: acquisition {
                  atoms {
                    ...southSequenceFields
                  }
                  time {
                    ...stepTimeFields
                  }
                }
                scienceS: science {
                  atoms {
                    ...southSequenceFields
                  }
                  time {
                    ...stepTimeFields
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
      object Observations {
        object Matches {
          type Config = ManualConfig
        }
      }
    }
  }
}
