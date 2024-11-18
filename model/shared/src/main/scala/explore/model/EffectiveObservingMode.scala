// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import clue.data.syntax.*
import cats.syntax.option.*
import lucuma.schemas.model.CentralWavelength
import lucuma.core.enums.GmosXBinning
import lucuma.core.enums.GmosYBinning
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosAmpGain
import lucuma.core.enums.GmosRoi
import cats.data.NonEmptyList
import lucuma.core.math.WavelengthDither
import lucuma.core.math.Offset
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
enum EffectiveObservingMode derives Eq:
  case GmosNorthLongSlit(
    grating:           GmosNorthGrating,
    filter:            Option[GmosNorthFilter],
    fpu:               GmosNorthFpu,
    centralWavelength: CentralWavelength,
    xBin:              GmosXBinning,
    yBin:              GmosYBinning,
    ampReadMode:       GmosAmpReadMode,
    ampGain:           GmosAmpGain,
    roi:               GmosRoi,
    wavelengthDithers: NonEmptyList[WavelengthDither],
    spatialOffsets:    NonEmptyList[Offset.Q]
  ) extends EffectiveObservingMode
  case GmosSouthLongSlit(
    grating:           GmosSouthGrating,
    filter:            Option[GmosSouthFilter],
    fpu:               GmosSouthFpu,
    centralWavelength: CentralWavelength,
    xBin:              GmosXBinning,
    yBin:              GmosYBinning,
    ampReadMode:       GmosAmpReadMode,
    ampGain:           GmosAmpGain,
    roi:               GmosRoi,
    wavelengthDithers: NonEmptyList[WavelengthDither],
    spatialOffsets:    NonEmptyList[Offset.Q]
  ) extends EffectiveObservingMode

  def toInput: ObservingModeInput = this match
    case GmosNorthLongSlit(
          grating,
          filter,
          fpu,
          centralWavelength,
          xBin,
          yBin,
          ampReadMode,
          ampGain,
          roi,
          wavelengthDithers,
          spatialOffsets
        ) =>
      ObservingModeInput(
        gmosNorthLongSlit = GmosNorthLongSlitInput(
          grating = grating.assign,
          filter = filter.orUnassign,
          fpu = fpu.assign,
          centralWavelength = centralWavelength.value.toInput.assign,
          explicitXBin = xBin.assign,
          explicitYBin = yBin.assign,
          explicitAmpReadMode = ampReadMode.assign,
          explicitAmpGain = ampGain.assign,
          explicitRoi = roi.assign,
          explicitWavelengthDithers = wavelengthDithers.map(_.toInput).toList.assign,
          explicitSpatialOffsets = spatialOffsets.map(_.toInput).toList.assign
        ).assign
      )
    case GmosSouthLongSlit(
          grating,
          filter,
          fpu,
          centralWavelength,
          xBin,
          yBin,
          ampReadMode,
          ampGain,
          roi,
          wavelengthDithers,
          spatialOffsets
        ) =>
      ObservingModeInput(
        gmosSouthLongSlit = GmosSouthLongSlitInput(
          grating = grating.assign,
          filter = filter.orUnassign,
          fpu = fpu.assign,
          centralWavelength = centralWavelength.value.toInput.assign,
          explicitXBin = xBin.assign,
          explicitYBin = yBin.assign,
          explicitAmpReadMode = ampReadMode.assign,
          explicitAmpGain = ampGain.assign,
          explicitRoi = roi.assign,
          explicitWavelengthDithers = wavelengthDithers.map(_.toInput).toList.assign,
          explicitSpatialOffsets = spatialOffsets.map(_.toInput).toList.assign
        ).assign
      )

object EffectiveObservingMode:
  def fromObservingMode(observingMode: ObservingMode): EffectiveObservingMode =
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
          explicitXBinning.getOrElse(defaultXBinning),
          explicitYBinning.getOrElse(defaultYBinning),
          explicitAmpReadMode.getOrElse(defaultAmpReadMode),
          explicitAmpGain.getOrElse(defaultAmpGain),
          explicitRoi.getOrElse(defaultRoi),
          explicitWavelengthDithers.getOrElse(defaultWavelengthDithers),
          explicitSpatialOffsets.getOrElse(defaultSpatialOffsets)
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
          explicitXBinning.getOrElse(defaultXBinning),
          explicitYBinning.getOrElse(defaultYBinning),
          explicitAmpReadMode.getOrElse(defaultAmpReadMode),
          explicitAmpGain.getOrElse(defaultAmpGain),
          explicitRoi.getOrElse(defaultRoi),
          explicitWavelengthDithers.getOrElse(defaultWavelengthDithers),
          explicitSpatialOffsets.getOrElse(defaultSpatialOffsets)
        )

  given Display[EffectiveObservingMode] = Display.byShortName:
    case GmosNorthLongSlit(
          grating,
          filter,
          fpu,
          centralWavelength,
          xBin,
          yBin,
          ampReadMode,
          ampGain,
          roi,
          wavelengthDithers,
          spatialOffsets
        ) =>
      s"GMOS-N Longslit ${grating.shortName} ${filter.map(_.shortName).getOrElse("None")} ${fpu.shortName} " +
        s"${centralWavelength.value.toNanometers}nm ${xBin.shortName}X${yBin.shortName} " +
        s"${ampReadMode.shortName} ${ampGain.shortName} ${roi.shortName} " +
        s"${wavelengthDithers.toList.map(ExploreModelValidators.ditherValidWedge.reverseGet).mkString(",")}nm " +
        s"${ExploreModelValidators.offsetQNELValidWedge.reverseGet(spatialOffsets.some)}arcsec"
    case GmosSouthLongSlit(
          grating,
          filter,
          fpu,
          centralWavelength,
          xBin,
          yBin,
          ampReadMode,
          ampGain,
          roi,
          wavelengthDithers,
          spatialOffsets
        ) =>
      s"GMOS-S Longslit ${grating.shortName} ${filter.map(_.shortName).getOrElse("None")} ${fpu.shortName} " +
        s"${centralWavelength.value.toNanometers}nm ${xBin.shortName}X${yBin.shortName} " +
        s"${ampReadMode.shortName} ${ampGain.shortName} ${roi.shortName} " +
        s"${wavelengthDithers.toList.map(ExploreModelValidators.ditherValidWedge.reverseGet).mkString(",")}nm " +
        s"${ExploreModelValidators.offsetQNELValidWedge.reverseGet(spatialOffsets.some)}arcsec"
