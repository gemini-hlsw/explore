// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.common

import clue.data.syntax.*
import explore.model.ScienceMode
import explore.model.ScienceModeAdvanced
import explore.model.ScienceModeBasic
import lucuma.core.math.*
import lucuma.schemas.ObservationDB.Types.*
import queries.schemas.odb.ODBConversions

trait ScienceConversions extends ODBConversions:
  extension (b: ScienceModeBasic.GmosNorthLongSlit)
    def toInput: ObservingModeInput =
      ObservingModeInput(
        // b.grating.assign, b.filter.orUnassign, b.fpu.assign
      )

  extension (b: ScienceModeBasic.GmosSouthLongSlit)
    def toInput: GmosSouthLongSlitInput =
      GmosSouthLongSlitInput(b.grating.assign, b.filter.orUnassign, b.fpu.assign)

  extension [A](o: Offset.Component[A])
    def toInput: OffsetComponentInput =
      OffsetComponentInput(microarcseconds = o.toAngle.toMicroarcseconds.assign)

  extension (a: ScienceModeAdvanced.GmosNorthLongSlit)
    def toInput: GmosNorthLongSlitInput =
      GmosNorthLongSlitInput(
        // a.overrideWavelength.map(_.toInput).orUnassign,
        // a.overrideGrating.orUnassign,
        // a.overrideFilter.orUnassign,
        // a.overrideFpu.orUnassign,
        // a.overrideExposureTimeMode.map(_.toInput).orUnassign,
        // a.explicitXBin.orUnassign,
        // a.explicitYBin.orUnassign,
        // a.explicitAmpReadMode.orUnassign,
        // a.explicitAmpGain.orUnassign,
        // a.explicitRoi.orUnassign,
        // a.explicitWavelengthDithers
        //   .map(_.toList.map(_.value))
        //   .orUnassign,
        // a.explicitSpatialOffsets.map(_.toList.map(_.toInput)).orUnassign
      )

  extension (a: ScienceModeAdvanced.GmosSouthLongSlit)
    def toInput: GmosSouthLongSlitInput =
      GmosSouthLongSlitInput(
        // a.overrideWavelength.map(_.toInput).orUnassign,
        // a.overrideGrating.orUnassign,
        // a.overrideFilter.orUnassign,
        // a.overrideFpu.orUnassign,
        // a.overrideExposureTimeMode.map(_.toInput).orUnassign,
        // a.explicitXBin.orUnassign,
        // a.explicitYBin.orUnassign,
        // a.explicitAmpReadMode.orUnassign,
        // a.explicitAmpGain.orUnassign,
        // a.explicitRoi.orUnassign,
        // a.explicitWavelengthDithers
        //   .map(_.toList.map(_.value))
        //   .orUnassign,
        // a.explicitSpatialOffsets.map(_.toList.map(_.toInput)).orUnassign
      )

  extension (b: ScienceMode)
    def toInput: ObservingModeInput = b match
      case ScienceMode.GmosNorthLongSlit(basic, advanced) =>
        ObservingModeInput(
          // gmosNorthLongSlit = GmosNorthLongSlitInput(
          //   basic = basic.toInput.assign,
          //   advanced = advanced.toInput.assign
          // ).assign
        )
      case ScienceMode.GmosSouthLongSlit(basic, advanced) =>
        ObservingModeInput(
          // gmosSouthLongSlit = GmosSouthLongSlitInput(
          //   basic = basic.toInput.assign,
          //   advanced = advanced.toInput.assign
          // ).assign
        )

object ScienceConversions extends ScienceConversions
