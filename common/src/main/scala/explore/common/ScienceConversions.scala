// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import clue.data.syntax.*
import explore.model.BasicConfiguration
import explore.model.DitherNanoMeters
import explore.model.ScienceMode
import lucuma.core.math.*
import lucuma.schemas.ObservationDB.Types.*
import queries.schemas.odb.ODBConversions

import scala.annotation.targetName

trait ScienceConversions extends ODBConversions:
  extension (d: DitherNanoMeters)
    def toInput: WavelengthDitherInput = WavelengthDitherInput(nanometers = d.value.assign)

  // The @targetName is needed to differentiate it from the Wavelength extension method. Both
  // are opaque types of `Quantity[Int, Picometer]`. This is fine at compile time without the
  // @targetName, but the js linker doesn't seem to be able to differentiate.
  extension (d: WavelengthDither)
    @targetName("WavelengthDither_toInput")
    def toInput = WavelengthDitherInput(picometers = d.toPicometers.value.assign)

  extension [A](o: Offset.Component[A])
    def toInput: OffsetComponentInput =
      OffsetComponentInput(microarcseconds = o.toAngle.toMicroarcseconds.assign)

  extension (o: ScienceMode.GmosNorthLongSlit)
    def toInput: GmosNorthLongSlitInput = GmosNorthLongSlitInput(
      grating = o.grating.assign,
      filter = o.filter.orUnassign,
      fpu = o.fpu.assign,
      centralWavelength = o.centralWavelength.value.toInput.assign,
      explicitXBin = o.explicitXBin.orUnassign,
      explicitYBin = o.explicitYBin.orUnassign,
      explicitAmpReadMode = o.explicitAmpReadMode.orUnassign,
      explicitAmpGain = o.explicitAmpGain.orUnassign,
      explicitRoi = o.explicitRoi.orUnassign,
      explicitWavelengthDithers =
        o.explicitWavelengthDithers.map(_.toList.map(_.toInput)).orUnassign,
      explicitSpatialOffsets = o.explicitSpatialOffsets.map(_.toList.map(_.toInput)).orUnassign
    )
  extension (o: ScienceMode.GmosSouthLongSlit)
    def toInput: GmosSouthLongSlitInput = GmosSouthLongSlitInput(
      grating = o.grating.assign,
      filter = o.filter.orUnassign,
      fpu = o.fpu.assign,
      centralWavelength = o.centralWavelength.value.toInput.assign,
      explicitXBin = o.explicitXBin.orUnassign,
      explicitYBin = o.explicitYBin.orUnassign,
      explicitAmpReadMode = o.explicitAmpReadMode.orUnassign,
      explicitAmpGain = o.explicitAmpGain.orUnassign,
      explicitRoi = o.explicitRoi.orUnassign,
      explicitWavelengthDithers =
        o.explicitWavelengthDithers.map(_.toList.map(_.toInput)).orUnassign,
      explicitSpatialOffsets = o.explicitSpatialOffsets.map(_.toList.map(_.toInput)).orUnassign
    )

  extension (b: ScienceMode)
    def toInput: ObservingModeInput = b match
      case o: ScienceMode.GmosNorthLongSlit =>
        ObservingModeInput(
          gmosNorthLongSlit = o.toInput.assign
        )
      case o: ScienceMode.GmosSouthLongSlit =>
        ObservingModeInput(
          gmosSouthLongSlit = o.toInput.assign
        )

  extension (i: BasicConfiguration)
    def toInput: ObservingModeInput = i match
      case o: BasicConfiguration.GmosNorthLongSlit =>
        ObservingModeInput(
          gmosNorthLongSlit = GmosNorthLongSlitInput(
            grating = o.grating.assign,
            filter = o.filter.orUnassign,
            fpu = o.fpu.assign,
            centralWavelength = o.centralWavelength.value.toInput.assign
          ).assign
        )
      case o: BasicConfiguration.GmosSouthLongSlit =>
        ObservingModeInput(
          gmosSouthLongSlit = GmosSouthLongSlitInput(
            grating = o.grating.assign,
            filter = o.filter.orUnassign,
            fpu = o.fpu.assign,
            centralWavelength = o.centralWavelength.value.toInput.assign
          ).assign
        )

object ScienceConversions extends ScienceConversions
