// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import clue.data.syntax.*
import explore.model.DitherNanoMeters
import explore.model.ScienceMode
import explore.model.ScienceModeInitial
import lucuma.core.math.*
import lucuma.schemas.ObservationDB.Types.*
import queries.schemas.odb.ODBConversions

trait ScienceConversions extends ODBConversions:
  extension (d: DitherNanoMeters)
    def toInput: WavelengthDitherInput = WavelengthDitherInput(nanometers = d.value.assign)

  extension [A](o: Offset.Component[A])
    def toInput: OffsetComponentInput =
      OffsetComponentInput(microarcseconds = o.toAngle.toMicroarcseconds.assign)

  extension (b: ScienceMode)
    def toInput: ObservingModeInput = b match
      case o: ScienceMode.GmosNorthLongSlit =>
        ObservingModeInput(
          gmosNorthLongSlit = GmosNorthLongSlitInput(
            grating = o.grating.assign,
            filter = o.filter.orUnassign,
            fpu = o.fpu.assign,
            centralWavelength = o.centralWavelength.value.toInput.assign,
            explicitXBin = o.explicitXBin.orUnassign,
            explicitYBin = o.explicitYBin.orUnassign,
            explicitAmpReadMode = o.explicitAmpReadMode.orUnassign,
            explicitAmpGain = o.explicitAmpGain.orUnassign,
            explicitWavelengthDithers =
              o.explicitWavelengthDithers.map(_.toList.map(_.toInput)).orUnassign,
            explicitSpatialOffsets =
              o.explicitSpatialOffsets.map(_.toList.map(_.toInput)).orUnassign
          ).assign
        )
      case o: ScienceMode.GmosSouthLongSlit =>
        ObservingModeInput(
          gmosSouthLongSlit = GmosSouthLongSlitInput(
            grating = o.grating.assign,
            filter = o.filter.orUnassign,
            fpu = o.fpu.assign,
            centralWavelength = o.centralWavelength.value.toInput.assign,
            explicitXBin = o.explicitXBin.orUnassign,
            explicitYBin = o.explicitYBin.orUnassign,
            explicitAmpReadMode = o.explicitAmpReadMode.orUnassign,
            explicitAmpGain = o.explicitAmpGain.orUnassign,
            explicitWavelengthDithers =
              o.explicitWavelengthDithers.map(_.toList.map(_.toInput)).orUnassign,
            explicitSpatialOffsets =
              o.explicitSpatialOffsets.map(_.toList.map(_.toInput)).orUnassign
          ).assign
        )

  extension (i: ScienceModeInitial)
    def toInput: ObservingModeInput = i match
      case o: ScienceModeInitial.GmosNorthLongSlit =>
        ObservingModeInput(
          gmosNorthLongSlit = GmosNorthLongSlitInput(
            grating = o.grating.assign,
            filter = o.filter.orUnassign,
            fpu = o.fpu.assign,
            centralWavelength = o.centralWavelength.value.toInput.assign
          ).assign
        )
      case o: ScienceModeInitial.GmosSouthLongSlit =>
        ObservingModeInput(
          gmosSouthLongSlit = GmosSouthLongSlitInput(
            grating = o.grating.assign,
            filter = o.filter.orUnassign,
            fpu = o.fpu.assign,
            centralWavelength = o.centralWavelength.value.toInput.assign
          ).assign
        )

object ScienceConversions extends ScienceConversions
