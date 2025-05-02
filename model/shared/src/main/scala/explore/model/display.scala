// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import eu.timepit.refined.cats.*
import explore.model.enums.WavelengthUnits
import explore.model.itc.ItcQueryProblem
import explore.modes.ItcInstrumentConfig
import lucuma.core.enums.*
import lucuma.core.math.BoundedInterval
import lucuma.core.math.BoundedInterval.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Wavelength
import lucuma.core.model.CloudExtinction
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.ImageQuality
import lucuma.core.model.Semester
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.syntax.display.*
import lucuma.core.util.Display
import lucuma.core.validation.InputValidSplitEpi
import lucuma.itc.GraphType
import lucuma.schemas.model.BasicConfiguration

import java.text.DecimalFormat
import scala.annotation.targetName

trait DisplayImplicits:
  given Display[AttachmentType] =
    Display.by(_.shortName, _.longName)

  given Display[Site] =
    Display.byShortName(_.shortName)

  given Display[TacGroup] =
    Display.byShortName(_.label)

  given Display[TacCategory] =
    Display.byShortName(_.label)

  given Display[ToOActivation] =
    Display.byShortName(_.label)

  given Display[Band] =
    Display.by(_.shortName, _.longName)

  given Display[SpectroscopyCapabilities] =
    Display.byShortName:
      case SpectroscopyCapabilities.NodAndShuffle => "Nod & Shuffle"
      case SpectroscopyCapabilities.Polarimetry   => "Polarimetry"
      case SpectroscopyCapabilities.Coronagraphy  => "Coronagraphy"

  given Display[FocalPlane] = Display.byShortName:
    case FocalPlane.SingleSlit   => "Single Slit"
    case FocalPlane.MultipleSlit => "Multiple Slits"
    case FocalPlane.IFU          => "IFU"

  given Display[ScienceMode] = Display.byShortName:
    case ScienceMode.Imaging      => "Imaging"
    case ScienceMode.Spectroscopy => "Spectroscopy"

  @targetName("given_Display_ImageQuality_Preset")
  given Display[ImageQuality.Preset] = Display.byShortName(_.toImageQuality.label)

  @targetName("given_Display_CloudExtinction_Preset")
  given Display[CloudExtinction.Preset] = Display.byShortName(_.toCloudExtinction.label)

  given Display[WaterVapor] = Display.byShortName(_.label)

  given Display[SkyBackground] = Display.byShortName(_.label)

  given Display[WavelengthUnits] = Display.byShortName(_.symbol)

  given Display[ConstraintSet] = Display.byShortName: cs =>
    val wv = if (cs.waterVapor === WaterVapor.Wet) "" else s" ${cs.waterVapor.label}"
    val er = cs.elevationRange match {
      case ElevationRange.ByAirMass(min, max)
          if min === ElevationRange.ByAirMass.DefaultMin && max === ElevationRange.ByAirMass.DefaultMax =>
        ""
      case ElevationRange.ByAirMass(min, max) if min === ElevationRange.ByAirMass.DefaultMin     =>
        f" AM<${max.value}%.1f"
      case ElevationRange.ByAirMass(min, max) if max === ElevationRange.ByAirMass.DefaultMax     =>
        f" ${min.value}%.1f<AM"
      case ElevationRange.ByAirMass(min, max)                                                    =>
        f" ${min.value}%.1f<AM<${max.value}%.1f"
      case ElevationRange.ByHourAngle(min, max) if min === ElevationRange.ByHourAngle.DefaultMin =>
        f" HA<${max.value}%.1f"
      case ElevationRange.ByHourAngle(min, max) if max === ElevationRange.ByHourAngle.DefaultMax =>
        f" ${min.value}%.1f<HA"
      case ElevationRange.ByHourAngle(min, max)                                                  =>
        f" ${min.value}%.1f<HA<${max.value}%.1f"
    }

    s"${cs.imageQuality.toImageQuality.label} ${cs.cloudExtinction.toCloudExtinction.label} ${cs.skyBackground.label}$wv$er"

  given Display[BrightnessValue] = Display.byShortName: x =>
    val f = new DecimalFormat("#.###")
    // We don't want scientific notation to kick in for magnitude units.
    // We assume it's magnitudes when x.abs >= 0.00001, since other units are usually
    // expressed in the order of e-18 or e-19.
    // We could make the format depend on the units, but that may be confusing for users
    // in case they want to type the value and then change the units.
    // Very large values (> 10000) are also forced into scientific notation.
    if (x.value.value.abs >= 0.00001 && x.value.value.abs <= 10000)
      f.format(x.value).replace("-0", "0")
    else
      InputValidSplitEpi.bigDecimalWithScientificNotation.reverseGet(x.value.value)

  given Display[StellarLibrarySpectrum] = Display.byShortName(_.sedSpectrum)

  import UnnormalizedSED.*
  given Display[UnnormalizedSED] = Display.byShortName:
    case StellarLibrary(librarySpectrum)          => librarySpectrum.shortName
    case CoolStarModel(temperature)               => s"Cool Star (${temperature.temperature.value} °K)"
    case Galaxy(galaxySpectrum)                   => galaxySpectrum.shortName
    case Planet(planetSpectrum)                   => planetSpectrum.shortName
    case Quasar(quasarSpectrum)                   => quasarSpectrum.shortName
    case HIIRegion(hiiRegionSpectrum)             => hiiRegionSpectrum.shortName
    case PlanetaryNebula(planetaryNebulaSpectrum) => planetaryNebulaSpectrum.shortName
    case PowerLaw(index)                          => s"Power Law ($index)"
    case BlackBody(temperature)                   => s"Black Body (${temperature.value} °K)"
    case UserDefined(_)                           => "User Defined"
    case UserDefinedAttachment(aid)               => s"User Defined (Attachment $aid)"

  given displaySpectralDefinition[T]: Display[SpectralDefinition[T]] = Display.byShortName:
    case SpectralDefinition.BandNormalized(Some(band), _) => band.shortName
    case SpectralDefinition.BandNormalized(_, _)          => "No SED"
    case SpectralDefinition.EmissionLines(_, _)           => "Emission Lines"

  given Display[GmosXBinning] = Display.by(_.shortName, _.longName)

  given Display[GmosYBinning] = Display.by(_.shortName, _.longName)

  given Display[GmosNorthGrating] = Display.byShortName(_.longName)

  given Display[GmosSouthGrating] = Display.byShortName(_.longName)

  given Display[GmosNorthFilter] = Display.byShortName(_.longName)

  given Display[GmosSouthFilter] = Display.byShortName(_.longName)

  given Display[GmosNorthFpu] = Display.byShortName(_.longName)

  given Display[GmosSouthFpu] = Display.byShortName(_.longName)

  given Display[GmosAmpReadMode] =
    Display.by(_.shortName, _.longName)

  given Display[GmosAmpGain] = Display.by(_.shortName, _.longName)

  given Display[GmosRoi] = Display.byShortName(_.longName)

  given Display[F2Disperser] = Display.by(_.shortName, _.longName)

  given Display[F2Filter] = Display.by(_.shortName, _.longName)

  given Display[F2Fpu] = Display.by(_.shortName, _.longName)

  given Display[F2ReadMode] = Display.by(_.shortName.capitalize, _.longName)

  given Display[F2Decker] = Display.by(_.shortName, _.longName)

  given Display[SequenceType] = Display.byShortName:
    case SequenceType.Acquisition => "Acquisition"
    case SequenceType.Science     => "Science"

  given Display[ScienceSubtype] = Display.byShortName:
    case ScienceSubtype.Classical          => "Classical"
    case ScienceSubtype.DirectorsTime      => "Director's Time"
    case ScienceSubtype.FastTurnaround     => "Fast Turnaround"
    case ScienceSubtype.LargeProgram       => "Large Program"
    case ScienceSubtype.PoorWeather        => "Poor Weather"
    case ScienceSubtype.Queue              => "Queue"
    case ScienceSubtype.DemoScience        => "Demo Science"
    case ScienceSubtype.SystemVerification => "System Verification"

  def wavelengthIntervalDisplay(units: WavelengthUnits): Display[BoundedInterval[Wavelength]] =
    Display.byShortName: interval =>
      List(interval.lower, interval.upper)
        .map { q =>
          units match
            case WavelengthUnits.Nanometers  =>
              val v = q.toNanometers.value.value.setScale(1, BigDecimal.RoundingMode.DOWN)
              "%.1f".format(v)
            case WavelengthUnits.Micrometers =>
              val v = q.toMicrometers.value.value.setScale(3, BigDecimal.RoundingMode.DOWN)
              "%.3f".format(v)
        }
        .mkString(" - ")

  given Display[CatalogName] = Display.byShortName:
    case CatalogName.Simbad => "SIMBAD"
    case CatalogName.Gaia   => "GAIA"
    case CatalogName.Import => "IMPORT"

  given Display[Semester] = Display.by(_.formatShort, _.format)

  given Display[RoleType] = Display.byShortName:
    case RoleType.Pi    => "Principal Investigator"
    case RoleType.NGO   => "NGO"
    case RoleType.Staff => "Staff"
    case RoleType.Admin => "Administrator"

  given Display[GraphType] = Display.byShortName:
    case GraphType.SignalGraph      => "Signal"
    case GraphType.SignalPixelGraph => "Pixel"
    case GraphType.S2NGraph         => "S/N"

  given Display[ItcQueryProblem] = Display.byShortName:
    case ItcQueryProblem.UnsupportedMode               => "Mode not supported"
    case ItcQueryProblem.MissingExposureTimeMode       => "Exposure time mode is missing"
    case ItcQueryProblem.MissingWavelength             => "Provide a wavelength"
    case ItcQueryProblem.MissingTargetInfo             => "Target information is missing"
    case ItcQueryProblem.MissingBrightness             => "Target brightness is missing"
    case ItcQueryProblem.SourceTooBright(halfWellTime) =>
      f"Source too bright, well half filled in $halfWellTime%.2f seconds"
    case ItcQueryProblem.GenericError(e)               => e

  given Display[ScienceBand] = Display.byShortName:
    case ScienceBand.Band1 => "Band-1"
    case ScienceBand.Band2 => "Band-2"
    case ScienceBand.Band3 => "Band-3"
    case ScienceBand.Band4 => "Band-4"

  given Display[EducationalStatus] = Display.byShortName:
    case EducationalStatus.PhD              => "PhD"
    case EducationalStatus.GradStudent      => "Grad Student"
    case EducationalStatus.UndergradStudent => "Undergrad Student"
    case EducationalStatus.Other            => "Other"

  given Display[Gender] = Display.byShortName:
    case Gender.Male         => "Male"
    case Gender.Female       => "Female"
    case Gender.Other        => "Other"
    case Gender.NotSpecified => "Not Specified"

  given Display[ProgramUserRole] = Display.by(
    {
      case ProgramUserRole.Pi               => "PI"
      case ProgramUserRole.Coi              => "CoI (full-access)"
      case ProgramUserRole.CoiRO            => "CoI (read-only)"
      case ProgramUserRole.SupportPrimary   => "Principal Support"
      case ProgramUserRole.SupportSecondary => "Additional Support"
      case ProgramUserRole.External         => "Data Only"
    },
    {
      case ProgramUserRole.Pi               => "Principal Investigator"
      case ProgramUserRole.Coi              => "Full-Access Co-Investigator"
      case ProgramUserRole.CoiRO            => "Read-Only Co-Investigator"
      case ProgramUserRole.SupportPrimary   => "Principal Support"
      case ProgramUserRole.SupportSecondary => "Additional Support"
      case ProgramUserRole.External         => "Data Only User"
    }
  )

  given Display[ConfigurationRequestStatus] = Display.byShortName(_.tag.capitalize)
  given Display[ObservationWorkflowState]   = Display.byShortName(_.tag.capitalize)

  given Display[ItcInstrumentConfig] = Display.byShortName:
    case ItcInstrumentConfig.GmosNorthSpectroscopy(grating, fpu, _, _) =>
      s"GMOS-N ${grating.shortName} ${fpu.shortName}"
    case ItcInstrumentConfig.GmosSouthSpectroscopy(grating, fpu, _, _) =>
      s"GMOS-S ${grating.shortName} ${fpu.shortName}"
    case _                                                             =>
      s"Unsupported configuration"

  given Display[BasicConfiguration] = Display.byShortName:
    case BasicConfiguration.GmosNorthLongSlit(grating, filter, fpu, cwl) =>
      val cwvStr    = "%.0fnm".format(cwl.value.toNanometers)
      val filterStr = filter.fold("None")(_.shortName)
      s"GMOS-N ${grating.shortName} @ $cwvStr $filterStr ${fpu.shortName}"
    case BasicConfiguration.GmosSouthLongSlit(grating, filter, fpu, cwl) =>
      val cwvStr    = "%.0fnm".format(cwl.value.toNanometers)
      val filterStr = filter.fold("None")(_.shortName)
      s"GMOS-S ${grating.shortName} @ $cwvStr $filterStr ${fpu.shortName}"
    case BasicConfiguration.F2LongSlit(disperser, _, fpu)                =>
      s"F2 ${disperser.shortName} ${fpu.shortName}"

object display extends DisplayImplicits
