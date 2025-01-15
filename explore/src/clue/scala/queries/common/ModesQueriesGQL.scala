// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package queries.common

import clue.GraphQLOperation
import clue.annotation.GraphQL
import lucuma.schemas.ObservationDB
import lucuma.core.enums.Instrument
import lucuma.core.syntax.string.*
import explore.modes.SpectroscopyModeRow

object ModesQueriesGQL:
  val supportedInstruments =
    List(Instrument.GmosNorth, Instrument.GmosSouth)
      .map(_.tag.toScreamingSnakeCase)
      .mkString("[", ", ", "]")

  @GraphQL
  trait SpectroscopyModes extends GraphQLOperation[ObservationDB]:
    val document: String = s"""
      query() {
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
        }
      }
    """

    object Data:
      type SpectroscopyConfigOptions = SpectroscopyModeRow
