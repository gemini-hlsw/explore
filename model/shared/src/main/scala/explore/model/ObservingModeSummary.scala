// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.kernel.Order
import cats.syntax.order.*
import clue.data.syntax.*
import explore.model.enums.PosAngleOptions
import lucuma.core.enums.F2Disperser
import lucuma.core.enums.F2Filter
import lucuma.core.enums.F2Fpu
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.util.Display
import lucuma.schemas.ObservationDB.Types.GmosNorthLongSlitInput
import lucuma.schemas.ObservationDB.Types.GmosSouthLongSlitInput
import lucuma.schemas.ObservationDB.Types.ObservingModeInput
import lucuma.schemas.model.CentralWavelength
import lucuma.schemas.model.ObservingMode
import lucuma.schemas.odb.input.*

// Observing mode with explicit values merged over defaults. Used for grouping observations by configuration.
enum ObservingModeSummary:
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
  case Flamingos2LongSlit(
    grating: F2Disperser,
    filter:  F2Filter,
    fpu:     F2Fpu
  ) extends ObservingModeSummary

  // Currently, everything is long slit and defaults to Average Parallactic.
  // But as we get new modes, Shortcut 3360 states:
  // Slit spectroscopy ->  Average Parallactic
  // MOS -> Fixed
  // Imaging -> Unconstrained
  // IFU -> 180 Flip
  def defaultPosAngleConstrait: PosAngleOptions = this match
    case GmosNorthLongSlit(_, _, _, _, _, _) => PosAngleOptions.AverageParallactic
    case GmosSouthLongSlit(_, _, _, _, _, _) => PosAngleOptions.AverageParallactic
    case Flamingos2LongSlit(_, _, _)         => PosAngleOptions.AverageParallactic

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
    case Flamingos2LongSlit(grating, filter, fpu)                                     =>
      // TODO ODB does not support F2 mode input yet
      ObservingModeInput()

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
            _,
            _,
            _,
            _,
            defaultAmpReadMode,
            explicitAmpReadMode,
            _,
            _,
            defaultRoi,
            explicitRoi,
            _,
            _,
            _,
            _
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
            _,
            _,
            _,
            _,
            defaultAmpReadMode,
            explicitAmpReadMode,
            _,
            _,
            defaultRoi,
            explicitRoi,
            _,
            _,
            _,
            _
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
      val cwvStr    = "%.1fnm".format(centralWavelength.value.toNanometers)
      val filterStr = filter.fold("None")(_.shortName)
      s"GMOS-N Longslit ${grating.shortName} @ $cwvStr $filterStr  ${fpu.shortName} ${ampReadMode.shortName} ${roi.shortName}"
    case GmosSouthLongSlit(grating, filter, fpu, centralWavelength, ampReadMode, roi) =>
      val cwvStr    = "%.1fnm".format(centralWavelength.value.toNanometers)
      val filterStr = filter.fold("None")(_.shortName)
      s"GMOS-S Longslit ${grating.shortName} @ $cwvStr $filterStr  ${fpu.shortName} ${ampReadMode.shortName} ${roi.shortName}"
    case Flamingos2LongSlit(grating, filter, fpu)                                     =>
      s"Flamingos2 Longslit ${grating.shortName} ${filter.shortName} ${fpu.shortName}"

  object GmosNorthLongSlit:
    given Order[GmosNorthLongSlit] =
      Order.by(x => (x.grating, x.filter, x.fpu, x.centralWavelength, x.ampReadMode, x.roi))

  object GmosSouthLongSlit:
    given Order[GmosSouthLongSlit] =
      Order.by(x => (x.grating, x.filter, x.fpu, x.centralWavelength, x.ampReadMode, x.roi))

  object Flamingos2LongSlit:
    given Order[Flamingos2LongSlit] =
      Order.by(x => (x.grating, x.filter, x.fpu))

  given Order[ObservingModeSummary] = Order.from:
    case (a @ GmosNorthLongSlit(_, _, _, _, _, _), b @ GmosNorthLongSlit(_, _, _, _, _, _)) =>
      a.compare(b)
    case (a @ GmosSouthLongSlit(_, _, _, _, _, _), b @ GmosSouthLongSlit(_, _, _, _, _, _)) =>
      a.compare(b)
    case (GmosNorthLongSlit(_, _, _, _, _, _), _)                                           => -1
    case _                                                                                  => 1
