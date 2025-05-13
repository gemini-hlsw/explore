// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
import explore.modes.SpectroscopyModeRow
import explore.modes.ImagingModeRow

object ModesQueriesGQL:

  @GraphQL
  trait ScienceModes extends GraphQLOperation[ObservationDB]:
    val document: String = s"""
      query($$supportedInstruments: [Instrument!]!) {
        spectroscopyConfigOptions(
          WHERE: {
            instrument: {
              IN: $$supportedInstruments
            }
          }
        ) {
          name
          instrument
          focalPlane
          capability
          adaptiveOptics
          wavelengthMin {
            picometers
          }
          wavelengthMax {
            picometers
          }
          wavelengthOptimal {
            picometers
          }
          wavelengthCoverage {
            picometers
          }
          resolution
          slitWidth {
            microarcseconds
          }
          slitLength {
            microarcseconds
          }
          gmosNorth {
            fpu
            grating
            filter
          }
          gmosSouth {
            fpu
            grating
            filter
          }
          flamingos2 {
            disperser
            filter
            fpu
          }
        }

        imagingConfigOptions(
          WHERE: {
            instrument: {
              IN: $$supportedInstruments
            }
          }
        ) {
          instrument
          adaptiveOptics
          fov {
            microarcseconds
          }
          gmosNorth {
            filter
          }
          gmosSouth {
            filter
          }
        }
      }
    """

    object Data:
      type SpectroscopyConfigOptions = SpectroscopyModeRow
      type ImagingConfigOptions      = ImagingModeRow
