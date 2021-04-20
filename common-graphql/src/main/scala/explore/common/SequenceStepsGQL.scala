// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.schemas.ObservationDB
import io.circe.Decoder
import java.time
import lucuma.core.math
import lucuma.core.math.Angle
import lucuma.core.math.Offset
// gql: import explore.model.decoders._
// gql: import explore.model.reusability._
// gql: import io.circe.refined._
// gql: import lucuma.ui.reusability._

object SequenceStepsGQL {

  @GraphQL
  trait SequenceSteps extends GraphQLOperation[ObservationDB] {
    val document = """
      query {
        observations(programId: "p-2", first:1000) {
          nodes {
            id
            name
            config {
              __typename
              instrument
              plannedTime {
                total {
                  microseconds
                }
              }              
              ... on GmosNorthConfig {
                scienceN:science {
                  atoms {
                    id
                    steps {
                      id
                      stepType
                      instrumentConfig {
                        exposure {
                          microseconds
                        }
                        grating {
                          disperser
                          wavelength {
                            picometers
                          }
                        }
                        fpu {
                          ... on GmosNorthBuiltinFpu {
                            builtin
                          }
                        }
                        filter
                        readout {
                          xBin
                          yBin
                        }
                        roi
                      }
                      stepConfig {
                        __typename
                        ... on Gcal {
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
                    }
                  }
                }
              }
              ... on GmosSouthConfig {
                scienceS:science {
                  atoms {
                    id
                    steps {
                      id
                      stepType
                      instrumentConfig {
                        exposure {
                          microseconds
                        }
                        grating {
                          disperser
                          wavelength {
                            picometers
                          }
                        }
                        fpu {
                          ... on GmosSouthBuiltinFpu {
                            builtin
                          }
                        }
                        filter
                        readout {
                          xBin
                          yBin
                        }
                        roi                        
                      }
                      stepConfig {
                        __typename
                        ... on Gcal {
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
                    }
                  }
                }
              }
            }
          }
        }
      }
    """

    implicit def offsetComponentDecoder[T]: Decoder[Offset.Component[T]] = Decoder.instance(c =>
      c.downField("microarcseconds")
        .as[Long]
        .map(Angle.signedMicroarcseconds.reverse.composeIso(Offset.Component.angle[T].reverse).get)
    )

    implicit val offsetDecoder: Decoder[Offset] = Decoder.instance(c =>
      for {
        p <- c.downField("p").as[Offset.P]
        q <- c.downField("q").as[Offset.Q]
      } yield Offset(p, q)
    )

    object Data {
      object Observations {
        object Nodes {
          object Config {
            object GmosNorthConfig {
              object ScienceN {
                object Atoms {
                  object Steps {
                    object StepType
                    object InstrumentConfig {
                      type Exposure = time.Duration
                      object Grating {
                        type Wavelength = math.Wavelength
                      }
                    }
                    object StepConfig       {
                      object Science {
                        type Offset = lucuma.core.math.Offset
                      }
                    }
                  }
                }
              }
            }

            object GmosSouthConfig {
              object ScienceS {
                object Atoms {
                  object Steps {
                    object StepType
                    object InstrumentConfig {
                      type Exposure = time.Duration
                      object Grating {
                        type Wavelength = math.Wavelength
                      }
                    }
                    object StepConfig       {
                      object Science {
                        type Offset = lucuma.core.math.Offset
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
}
