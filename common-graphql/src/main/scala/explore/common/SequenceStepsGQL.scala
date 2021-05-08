// Copyright (c) 2016-2021 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import explore.schemas.ObservationDB
import io.circe.Decoder
import lucuma.core.enum
import lucuma.core.math
import lucuma.core.math.Angle
import lucuma.core.math.Offset
import lucuma.core.model.Atom
import lucuma.core.model.Step

import java.time
// gql: import explore.model.decoders._
// gql: import explore.model.reusability._
// gql: import io.circe.refined._
// gql: import lucuma.ui.reusability._

object SequenceStepsGQL {

  @GraphQL
  trait SequenceSteps extends GraphQLOperation[ObservationDB] {
    val document = """
      query {
        observations(programId: "p-2", first: 1) {
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
                acquisitionN:acquisition {
                  ...northSequenceFields
                }
                scienceN:science {
                  ...northSequenceFields
                }
              }
              ... on GmosSouthConfig {
                acquisitionS: acquisition {
                  ...southSequenceFields
                }
                scienceS:science {
                  ...southSequenceFields
                }
              }
            }
          }
        }
      }

      fragment northSequenceFields on GmosNorthSequence {
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

      fragment southSequenceFields on GmosSouthSequence {
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

    sealed trait SeqSite {
      type Disperser
      type Fpu
      type Filter
    }
    object SeqSite       {
      sealed trait North extends SeqSite {
        type Disperser = enum.GmosNorthDisperser
        type Fpu       = enum.GmosNorthFpu
        type Filter    = enum.GmosNorthFilter
      }
      sealed trait South extends SeqSite {
        type Disperser = enum.GmosSouthDisperser
        type Fpu       = enum.GmosSouthFpu
        type Filter    = enum.GmosSouthFilter
      }
    }

    trait SeqGrating[Site <: SeqSite]          {
      val disperser: Site#Disperser
      val wavelength: math.Wavelength
    }
    trait SeqFpu[Site <: SeqSite]              {
      val builtin: Site#Fpu
    }
    trait SeqReadout                           {
      val xBin: enum.GmosXBinning
      val yBin: enum.GmosYBinning
    }
    trait SeqInstrumentConfig[Site <: SeqSite] {
      val exposure: time.Duration
      val grating: Option[SeqGrating[Site]]
      val fpu: Option[SeqFpu[Site]]
      val filter: Option[Site#Filter]
      val readout: SeqReadout
      val roi: enum.GmosRoi
    }
    sealed trait SeqStepConfig
    object SeqStepConfig                       {
      trait SeqScienceStep extends SeqStepConfig {
        val offset: Offset
      }
    }
    trait SeqStep[Site <: SeqSite]             {
      val id: Step.Id
      val instrumentConfig: SeqInstrumentConfig[Site]
      val stepType: enum.StepType
      val stepConfig: SeqStepConfig
    }
    trait SeqAtom[Site <: SeqSite]             {
      val id: Atom.Id
      val steps: List[SeqStep[Site]]
    }
    trait Sequence[Site <: SeqSite]            {
      val atoms: List[SeqAtom[Site]]
    }

    object Data {
      object Observations {
        object Nodes {
          object Config {
            object GmosNorthConfig {
              trait AcquisitionN extends Sequence[SeqSite.North]
              object AcquisitionN {
                trait Atoms extends SeqAtom[SeqSite.North]
                object Atoms {
                  trait Steps extends SeqStep[SeqSite.North]
                  object Steps {
                    // object StepType
                    trait InstrumentConfig extends SeqInstrumentConfig[SeqSite.North]
                    object InstrumentConfig {
                      type Exposure = time.Duration
                      trait Grating extends SeqGrating[SeqSite.North]
                      object Grating {
                        type Wavelength = math.Wavelength
                      }
                      trait Fpu extends SeqFpu[SeqSite.North]
                      trait Readout extends SeqReadout
                    }
                    trait StepConfig extends SeqStepConfig
                    object StepConfig       {
                      trait Science extends SeqStepConfig.SeqScienceStep
                      object Science {
                        type Offset = lucuma.core.math.Offset
                      }
                    }
                  }
                }
              }
              trait ScienceN extends Sequence[SeqSite.North]
              object ScienceN     {
                trait Atoms extends SeqAtom[SeqSite.North]
                object Atoms {
                  trait Steps extends SeqStep[SeqSite.North]
                  object Steps {
                    // object StepType
                    trait InstrumentConfig extends SeqInstrumentConfig[SeqSite.North]
                    object InstrumentConfig {
                      type Exposure = time.Duration
                      trait Grating extends SeqGrating[SeqSite.North]
                      object Grating {
                        type Wavelength = math.Wavelength
                      }
                      trait Fpu extends SeqFpu[SeqSite.North]
                      trait Readout extends SeqReadout
                    }
                    trait StepConfig extends SeqStepConfig
                    object StepConfig       {
                      trait Science extends SeqStepConfig.SeqScienceStep
                      object Science {
                        type Offset = lucuma.core.math.Offset
                      }
                    }
                  }
                }
              }
            }

            object GmosSouthConfig {
              trait AcquisitionS extends Sequence[SeqSite.South]
              object AcquisitionS {
                trait Atoms extends SeqAtom[SeqSite.South]
                object Atoms {
                  trait Steps extends SeqStep[SeqSite.South]
                  object Steps {
                    // object StepType
                    trait InstrumentConfig extends SeqInstrumentConfig[SeqSite.South]
                    object InstrumentConfig {
                      type Exposure = time.Duration
                      trait Grating extends SeqGrating[SeqSite.South]
                      object Grating {
                        type Wavelength = math.Wavelength
                      }
                      trait Fpu extends SeqFpu[SeqSite.South]
                      trait Readout extends SeqReadout
                    }
                    trait StepConfig extends SeqStepConfig
                    object StepConfig       {
                      trait Science extends SeqStepConfig.SeqScienceStep
                      object Science {
                        type Offset = lucuma.core.math.Offset
                      }
                    }
                  }
                }
              }
              trait ScienceS extends Sequence[SeqSite.South]
              object ScienceS     {
                trait Atoms extends SeqAtom[SeqSite.South]
                object Atoms {
                  trait Steps extends SeqStep[SeqSite.South]
                  object Steps {
                    // object StepType
                    trait InstrumentConfig extends SeqInstrumentConfig[SeqSite.South]
                    object InstrumentConfig {
                      type Exposure = time.Duration
                      trait Grating extends SeqGrating[SeqSite.South]
                      object Grating {
                        type Wavelength = math.Wavelength
                      }
                      trait Fpu extends SeqFpu[SeqSite.South]
                      trait Readout extends SeqReadout
                    }
                    trait StepConfig extends SeqStepConfig
                    object StepConfig       {
                      trait Science extends SeqStepConfig.SeqScienceStep
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
