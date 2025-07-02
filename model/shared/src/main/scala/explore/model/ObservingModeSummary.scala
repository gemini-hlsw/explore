// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.data.NonEmptyList
import cats.kernel.Order
import cats.syntax.order.*
import clue.data.syntax.*
import lucuma.core.enums.Flamingos2Disperser
import lucuma.core.enums.Flamingos2Filter
import lucuma.core.enums.Flamingos2Fpu
import lucuma.core.enums.GmosAmpReadMode
import lucuma.core.enums.GmosNorthFilter
import lucuma.core.enums.GmosNorthFpu
import lucuma.core.enums.GmosNorthGrating
import lucuma.core.enums.GmosRoi
import lucuma.core.enums.GmosSouthFilter
import lucuma.core.enums.GmosSouthFpu
import lucuma.core.enums.GmosSouthGrating
import lucuma.core.enums.ObservingModeType
import lucuma.core.util.Display
import lucuma.schemas.ObservationDB.Types.Flamingos2LongSlitInput
import lucuma.schemas.ObservationDB.Types.GmosNorthImagingInput
import lucuma.schemas.ObservationDB.Types.GmosNorthLongSlitInput
import lucuma.schemas.ObservationDB.Types.GmosSouthImagingInput
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
    grating: Flamingos2Disperser,
    filter:  Flamingos2Filter,
    fpu:     Flamingos2Fpu
  ) extends ObservingModeSummary
  case GmosNorthImaging(
    filters:     NonEmptyList[GmosNorthFilter],
    ampReadMode: GmosAmpReadMode,
    roi:         GmosRoi
  ) extends ObservingModeSummary
  case GmosSouthImaging(
    filters:     NonEmptyList[GmosSouthFilter],
    ampReadMode: GmosAmpReadMode,
    roi:         GmosRoi
  ) extends ObservingModeSummary

  def obsModeType: ObservingModeType = this match
    case GmosNorthLongSlit(_, _, _, _, _, _) => ObservingModeType.GmosNorthLongSlit
    case GmosSouthLongSlit(_, _, _, _, _, _) => ObservingModeType.GmosSouthLongSlit
    case Flamingos2LongSlit(_, _, _)         => ObservingModeType.Flamingos2LongSlit
    case GmosNorthImaging(_, _, _)           => ObservingModeType.GmosNorthImaging
    case GmosSouthImaging(_, _, _)           => ObservingModeType.GmosSouthImaging

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
    case Flamingos2LongSlit(disperser, filter, fpu)                                   =>
      ObservingModeInput(
        flamingos2LongSlit = Flamingos2LongSlitInput(
          disperser = disperser.assign,
          filter = filter.assign,
          fpu = fpu.assign
        ).assign
      )
    case GmosNorthImaging(filters, ampReadMode, roi)                                  =>
      ObservingModeInput(
        gmosNorthImaging = GmosNorthImagingInput(
          filters = filters.toList.assign,
          explicitAmpReadMode = ampReadMode.assign,
          explicitRoi = roi.assign
        ).assign
      )
    case GmosSouthImaging(filters, ampReadMode, roi)                                  =>
      ObservingModeInput(
        gmosSouthImaging = GmosSouthImagingInput(
          filters = filters.toList.assign,
          explicitAmpReadMode = ampReadMode.assign,
          explicitRoi = roi.assign
        ).assign
      )

object ObservingModeSummary:
  def fromObservingMode(observingMode: ObservingMode): ObservingModeSummary =
    observingMode match
      case n: ObservingMode.GmosNorthLongSlit  =>
        GmosNorthLongSlit(
          n.grating,
          n.filter,
          n.fpu,
          n.centralWavelength,
          n.ampReadMode,
          n.roi
        )
      case s: ObservingMode.GmosSouthLongSlit  =>
        GmosSouthLongSlit(
          s.grating,
          s.filter,
          s.fpu,
          s.centralWavelength,
          s.ampReadMode,
          s.roi
        )
      case f: ObservingMode.Flamingos2LongSlit =>
        Flamingos2LongSlit(f.disperser, f.filter, f.fpu)
      case n: ObservingMode.GmosNorthImaging   =>
        GmosNorthImaging(n.filters, n.ampReadMode, n.roi)
      case s: ObservingMode.GmosSouthImaging   =>
        GmosSouthImaging(s.filters, s.ampReadMode, s.roi)

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
    case GmosNorthImaging(filters, ampReadMode, roi)                                  =>
      val filterStr = filters.map(_.shortName).toList.mkString(", ")
      s"GMOS-N Imaging $filterStr ${ampReadMode.shortName} ${roi.shortName}"
    case GmosSouthImaging(filters, ampReadMode, roi)                                  =>
      val filterStr = filters.map(_.shortName).toList.mkString(", ")
      s"GMOS-S Imaging $filterStr ${ampReadMode.shortName} ${roi.shortName}"

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
