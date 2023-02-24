// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package explore.model

import cats.syntax.all.*
import eu.timepit.refined.cats.*
import lucuma.core.enums.*
import lucuma.core.math.BoundedInterval
import lucuma.core.math.BoundedInterval.*
import lucuma.core.math.BrightnessValue
import lucuma.core.math.Wavelength
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ElevationRange
import lucuma.core.model.Semester
import lucuma.core.model.SpectralDefinition
import lucuma.core.model.UnnormalizedSED
import lucuma.core.syntax.display.*
import lucuma.core.util.Display
import lucuma.core.validation.InputValidSplitEpi

import java.text.DecimalFormat

trait DisplayImplicits:
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
    Display.byShortName {
      case SpectroscopyCapabilities.NodAndShuffle => "Nod & Shuffle"
      case SpectroscopyCapabilities.Polarimetry   => "Polarimetry"
      case SpectroscopyCapabilities.Coronagraphy  => "Coronagraphy"
    }

  given Display[FocalPlane] = Display.byShortName {
    case FocalPlane.SingleSlit   => "Single Slit"
    case FocalPlane.MultipleSlit => "Multiple Slits"
    case FocalPlane.IFU          => "IFU"
  }

  given Display[ScienceMode] = Display.byShortName {
    case ScienceMode.Imaging      => "Imaging"
    case ScienceMode.Spectroscopy => "Spectroscopy"
  }

  given Display[ImageQuality] = Display.byShortName(_.label)

  given Display[CloudExtinction] = Display.byShortName(_.label)

  given Display[WaterVapor] = Display.byShortName(_.label)

  given Display[SkyBackground] = Display.byShortName(_.label)

  given Display[ConstraintSet] = Display.byShortName { cs =>
    val wv = if (cs.waterVapor === WaterVapor.Wet) "" else s" ${cs.waterVapor.label}"
    val er = cs.elevationRange match {
      case ElevationRange.AirMass(min, max)
          if min === ElevationRange.AirMass.DefaultMin && max === ElevationRange.AirMass.DefaultMax =>
        ""
      case ElevationRange.AirMass(min, max) if min === ElevationRange.AirMass.DefaultMin     =>
        f" AM<${max.value}%.1f"
      case ElevationRange.AirMass(min, max) if max === ElevationRange.AirMass.DefaultMax     =>
        f" ${min.value}%.1f<AM"
      case ElevationRange.AirMass(min, max)                                                  =>
        f" ${min.value}%.1f<AM<${max.value}%.1f"
      case ElevationRange.HourAngle(min, max) if min === ElevationRange.HourAngle.DefaultMin =>
        f" HA<${max.value}%.1f"
      case ElevationRange.HourAngle(min, max) if max === ElevationRange.HourAngle.DefaultMax =>
        f" ${min.value}%.1f<HA"
      case ElevationRange.HourAngle(min, max)                                                =>
        f" ${min.value}%.1f<HA<${max.value}%.1f"
    }

    s"${cs.imageQuality.label} ${cs.cloudExtinction.label} ${cs.skyBackground.label}$wv$er"
  }

  given Display[BrightnessValue] = Display.byShortName { x =>
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
  }

  given Display[StellarLibrarySpectrum] = Display.byShortName(_.sedSpectrum)

  import UnnormalizedSED.*
  given Display[UnnormalizedSED] = Display.byShortName {
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
  }

  given displaySpectralDefinition[T]: Display[SpectralDefinition[T]] = Display.byShortName {
    case SpectralDefinition.BandNormalized(Some(band), _) => band.shortName
    case SpectralDefinition.BandNormalized(_, _)          => "No SED"
    case SpectralDefinition.EmissionLines(_, _)           => "Emission Lines"
  }

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

  given Display[SequenceType] = Display.byShortName(_ match
    case SequenceType.Acquisition => "Acquisition"
    case SequenceType.Science     => "Science"
  )

  given Display[BoundedInterval[Wavelength]] = Display.byShortName(interval =>
    List(interval.lower, interval.upper)
      .map(q =>
        "%.3f".format(q.toMicrometers.value.value.setScale(3, BigDecimal.RoundingMode.DOWN))
      )
      .mkString(" - ")
  )

  given Display[CatalogName] = Display.byShortName {
    case CatalogName.Simbad => "SIMBAD"
    case CatalogName.Gaia   => "GAIA"
    case CatalogName.Import => "IMPORT"
  }

  given Display[Semester] = Display.by(_.formatShort, _.format)

object display extends DisplayImplicits
