// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import clue.data.syntax.*
import cats.syntax.option.*
import lucuma.schemas.model.CentralWavelength
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import cats.Eq
import cats.derived.*
import lucuma.core.util.Display
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.ObservationDB.Types.ObservingModeInput
import lucuma.schemas.ObservationDB.Types.GmosNorthLongSlitInput
import lucuma.schemas.odb.input.*
import lucuma.schemas.ObservationDB.Types.GmosSouthLongSlitInput

// Observing mode with explicit values merged over defaults. Used for grouping observations by configuration.
enum ObservingModeSummary derives Eq:
  case GmosNorthLongSlit(
    grating:           GmosNorthGrating,
    filter:            Option[GmosNorthFilter],
    fpu:               GmosNorthFpu,
    centralWavelength: CentralWavelength,
    ampReadMode:       GmosAmpReadMode,
    roi:               GmosRoi
  ) extends ObservingModeSummary
  case GmosSouthLongSlit(
    grating:           GmosSouthGrating,
    filter:            Option[GmosSouthFilter],
    fpu:               GmosSouthFpu,
    centralWavelength: CentralWavelength,
    ampReadMode:       GmosAmpReadMode,
    roi:               GmosRoi
  ) extends ObservingModeSummary

  def toInput: ObservingModeInput = this match
    case GmosNorthLongSlit(grating, filter, fpu, centralWavelength, ampReadMode, roi) =>
      ObservingModeInput(
        gmosNorthLongSlit = GmosNorthLongSlitInput(
          grating = grating.assign,
          filter = filter.orUnassign,
          fpu = fpu.assign,
          centralWavelength = centralWavelength.value.toInput.assign,
          explicitAmpReadMode = ampReadMode.assign,
          explicitRoi = roi.assign
        ).assign
      )
    case GmosSouthLongSlit(grating, filter, fpu, centralWavelength, ampReadMode, roi) =>
      ObservingModeInput(
        gmosSouthLongSlit = GmosSouthLongSlitInput(
          grating = grating.assign,
          filter = filter.orUnassign,
          fpu = fpu.assign,
          centralWavelength = centralWavelength.value.toInput.assign,
          explicitAmpReadMode = ampReadMode.assign,
          explicitRoi = roi.assign
        ).assign
      )

object ObservingModeSummary:
  def fromObservingMode(observingMode: ObservingMode): ObservingModeSummary =
    observingMode match
      case ObservingMode.GmosNorthLongSlit(
            _,
            grating,
            _,
            filter,
            _,
            fpu,
            _,
            centralWavelength,
            defaultXBinning,
            explicitXBinning,
            defaultYBinning,
            explicitYBinning,
            defaultAmpReadMode,
            explicitAmpReadMode,
            defaultAmpGain,
            explicitAmpGain,
            defaultRoi,
            explicitRoi,
            defaultWavelengthDithers,
            explicitWavelengthDithers,
            defaultSpatialOffsets,
            explicitSpatialOffsets
          ) =>
        GmosNorthLongSlit(
          grating,
          filter,
          fpu,
          centralWavelength,
          explicitAmpReadMode.getOrElse(defaultAmpReadMode),
          explicitRoi.getOrElse(defaultRoi)
        )
      case ObservingMode.GmosSouthLongSlit(
            _,
            grating,
            _,
            filter,
            _,
            fpu,
            _,
            centralWavelength,
            defaultXBinning,
            explicitXBinning,
            defaultYBinning,
            explicitYBinning,
            defaultAmpReadMode,
            explicitAmpReadMode,
            defaultAmpGain,
            explicitAmpGain,
            defaultRoi,
            explicitRoi,
            defaultWavelengthDithers,
            explicitWavelengthDithers,
            defaultSpatialOffsets,
            explicitSpatialOffsets
          ) =>
        GmosSouthLongSlit(
          grating,
          filter,
          fpu,
          centralWavelength,
          explicitAmpReadMode.getOrElse(defaultAmpReadMode),
          explicitRoi.getOrElse(defaultRoi)
        )

  given Display[ObservingModeSummary] = Display.byShortName:
    case GmosNorthLongSlit(grating, filter, fpu, centralWavelength, ampReadMode, roi) =>
      s"GMOS-N Longslit ${grating.shortName} ${filter.map(_.shortName).getOrElse("None")} ${fpu.shortName} " +
        s"${centralWavelength.value.toNanometers}nm ${ampReadMode.shortName} ${roi.shortName}"
    case GmosSouthLongSlit(grating, filter, fpu, centralWavelength, ampReadMode, roi) =>
      s"GMOS-S Longslit ${grating.shortName} ${filter.map(_.shortName).getOrElse("None")} ${fpu.shortName} " +
        s"${centralWavelength.value.toNanometers}nm ${ampReadMode.shortName} ${roi.shortName}"
