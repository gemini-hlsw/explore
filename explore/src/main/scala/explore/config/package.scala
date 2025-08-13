// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.config

import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import explore.model.ExploreModelValidators
import explore.model.ScienceRequirements
import explore.model.enums.WavelengthUnits
import explore.modes.ItcInstrumentConfig
import explore.modes.ModeCommonWavelengths
import explore.modes.ModeSlitSize
import explore.modes.ModeWavelength
import explore.modes.SlitLength
import explore.modes.SpectroscopyModeRow
import explore.modes.SpectroscopyModesMatrix
import japgolly.scalajs.react.*
import lucuma.core.math.Wavelength
import lucuma.core.math.WavelengthDelta
import lucuma.core.optics.Format
import lucuma.core.syntax.string.parseBigDecimalOption
import lucuma.core.validation.*
import lucuma.refined.*
import lucuma.schemas.model.ObservingMode
import lucuma.ui.input.ChangeAuditor
import lucuma.ui.reusability.given

trait ConfigurationFormats:
  private lazy val angleArcsecondsBaseAuditor = ChangeAuditor
    .fromInputValidWedge(ExploreModelValidators.decimalArcsecondsValidWedge)
    .allow(s => s === "0" || s === "0.")
  lazy val angleArcsecondsChangeAuditor       = angleArcsecondsBaseAuditor
    .decimal(2.refined)
    .optional

  lazy val angleArcsecsFormat   = ExploreModelValidators.decimalArcsecondsValidWedge.optional
  lazy val wvMicroInput         = ExploreModelValidators.wavelengthMicroValidWedge.optional
  lazy val wvNanoInput          = ExploreModelValidators.wavelengthNanoValidWedge.optional
  lazy val wvAngstromInput      = ExploreModelValidators.wavelengthAngstromValidWedge.optional
  lazy val wvDeltaMicroInput    = ExploreModelValidators.wavelengthMicroDeltaValidWedge.optional
  lazy val wvDeltaNanoInput     = ExploreModelValidators.wavelengthNanoDeltaValidWedge.optional
  lazy val wvDeltaAngstromInput = ExploreModelValidators.wavelengthAngstromDeltaValidWedge.optional

  private def bdFormat(scale: Int): Format[String, BigDecimal] =
    Format[String, BigDecimal](parseBigDecimalOption, _.setScale(scale).toString)

  extension (u: WavelengthUnits)
    def toAuditor: ChangeAuditor =
      u match
        case WavelengthUnits.Micrometers => ChangeAuditor.posBigDecimal(3.refined)
        case WavelengthUnits.Nanometers  => ChangeAuditor.posBigDecimal(1.refined)
        case WavelengthUnits.Angstroms   => ChangeAuditor.posInt

    def toSNAuditor: ChangeAuditor = toAuditor

    def toInputWedge: InputValidWedge[Option[Wavelength]] =
      u match
        case WavelengthUnits.Micrometers => wvMicroInput
        case WavelengthUnits.Nanometers  => wvNanoInput
        case WavelengthUnits.Angstroms   => wvAngstromInput

    def toInputFormat: InputValidFormat[Wavelength] =
      u match
        case WavelengthUnits.Micrometers => ExploreModelValidators.wavelengthMicroValidWedge
        case WavelengthUnits.Nanometers  => ExploreModelValidators.wavelengthNanoValidWedge
        case WavelengthUnits.Angstroms   => ExploreModelValidators.wavelengthAngstromValidWedge

    def format: Format[String, Wavelength] =
      u match
        case WavelengthUnits.Micrometers => bdFormat(2).andThen(Wavelength.decimalMicrometers)
        case WavelengthUnits.Nanometers  => bdFormat(1).andThen(Wavelength.decimalNanometers)
        case WavelengthUnits.Angstroms   => bdFormat(0).andThen(Wavelength.decimalAngstroms)

    def toDeltaInputWedge: InputValidWedge[Option[WavelengthDelta]] =
      u match
        case WavelengthUnits.Micrometers => wvDeltaMicroInput
        case WavelengthUnits.Nanometers  => wvDeltaNanoInput
        case WavelengthUnits.Angstroms   => wvDeltaAngstromInput

object ConfigurationFormats extends ConfigurationFormats

case class ModeData private (
  resolution: PosInt,
  λmin:       ModeWavelength,
  λmax:       ModeWavelength,
  λdelta:     WavelengthDelta
) extends ModeCommonWavelengths

object ModeData {
  def build(row: SpectroscopyModeRow, reqWavelength: Option[Wavelength]): Option[ModeData] =
    reqWavelength.flatMap { rw =>
      if (rw >= row.λmin.value && rw <= row.λmax.value)
        ModeData(
          row.resolution,
          row.λmin,
          row.λmax,
          row.λdelta
        ).some
      else
        none
    }
}

def useModeData(
  confMatrix:               SpectroscopyModesMatrix,
  spectroscopyRequirements: ScienceRequirements.Spectroscopy,
  obsMode:                  ObservingMode
): HookResult[Option[ModeData]] =
  // a reusablity based only on what is used here
  given Reusability[ObservingMode] = Reusability:
    // TODO: change to named tuples with scala 3.7
    case (x: ObservingMode.GmosNorthLongSlit, y: ObservingMode.GmosNorthLongSlit)   =>
      x.grating === y.grating && x.filter === y.filter && x.fpu === y.fpu
    case (x: ObservingMode.GmosSouthLongSlit, y: ObservingMode.GmosSouthLongSlit)   =>
      x.grating === y.grating && x.filter === y.filter && x.fpu === y.fpu
    case (x: ObservingMode.Flamingos2LongSlit, y: ObservingMode.Flamingos2LongSlit) =>
      x.disperser === y.disperser && x.filter === y.filter && x.fpu === y.fpu
    case _                                                                          => false

  def findMatrixDataFromRow(
    reqsWavelength: Option[Wavelength],
    row:            SpectroscopyModeRow
  ): Option[ModeData] =
    reqsWavelength.flatMap(_ =>
      (obsMode, row.instrument) match
        // TODO: change to named tuples with scala 3.7
        case (m: ObservingMode.GmosNorthLongSlit,
              ItcInstrumentConfig.GmosNorthSpectroscopy(rGrating, rFpu, rFilter, _)
            ) if m.grating === rGrating && m.filter === rFilter && m.fpu === rFpu =>
          ModeData.build(row, reqsWavelength)
        case (m: ObservingMode.GmosSouthLongSlit,
              ItcInstrumentConfig.GmosSouthSpectroscopy(rGrating, rFpu, rFilter, _)
            ) if m.grating === rGrating && m.filter === rFilter && m.fpu === rFpu =>
          ModeData.build(row, reqsWavelength)
        case (m: ObservingMode.Flamingos2LongSlit,
              ItcInstrumentConfig.Flamingos2Spectroscopy(rGrating, rFilter, rFpu)
            ) if m.disperser === rGrating && m.filter === rFilter && m.fpu === rFpu =>
          ModeData.build(row, reqsWavelength)
        case _ => none
    )

  def findMatrixData(
    reqsWavelength: Option[Wavelength],
    rows:           List[SpectroscopyModeRow]
  ): Option[ModeData] =
    rows.collectFirstSome(row => findMatrixDataFromRow(reqsWavelength, row))
  for {
    // filter the spectroscopy matrix by the requirements that don't get overridden
    // by the advanced config (wavelength, for example).
    rows     <- useMemo(
                  (spectroscopyRequirements.focalPlane,
                   spectroscopyRequirements.capability,
                   spectroscopyRequirements.focalPlaneAngle,
                   spectroscopyRequirements.resolution,
                   spectroscopyRequirements.wavelengthCoverage,
                   confMatrix.matrix.length
                  )
                ) { (fp, cap, fpa, res, rng, _) =>
                  confMatrix.filtered(
                    focalPlane = fp,
                    capability = cap,
                    slitLength = fpa.map(s => SlitLength(ModeSlitSize(s))),
                    resolution = res,
                    range = rng
                  )
                }
    // Try to find the mode row from the spectroscopy matrix
    modeData <-
      useMemo((spectroscopyRequirements.wavelength, rows, obsMode)) { (reqsWavelength, rows, _) =>
        findMatrixData(reqsWavelength, rows)
      }
  } yield modeData
